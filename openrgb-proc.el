;;; openrgb-proc.el --- Process management for OpenRGB client -*- lexical-binding: t -*-

;; Copyright (C) 2024 B. Szilvasy

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
 
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This file implements the process management code for the OpenRGB
;; client, specifically, sending and receiving raw packets.

;;; Code:

(eval-when-compile
  (require 'openrgb-macs))
(require 'openrgb-core)
(require 'promise)
(require 'bindat)
(require 'openrgb-types)

(defun openrgb-packet-has-id (packet id)
  "Return non-nil if PACKET's ID matches."
  (equal id (alist-get 'id packet)))

(defun openrgb--proc-recv (proc id type)
  "Receive on PROC a packet of TYPE with ID.

TYPE is a `bindat' type.

Returns an `promise' with the value."
  (promise-then
   (openrgb--pop-packet proc #'openrgb-packet-has-id id)
   (lambda (packet)
     (openrgb-with-process-buffer proc
       (bindat-unpack type (alist-get 'payload packet))))))

(defun openrgb--proc-send (proc id device value type)
  "Send a message to PROC, with payload VALUE of TYPE.

ID is the packet identifier to send with, and DEVICE is the
device index to include."
  (openrgb-with-process-buffer proc
    (let* ((packed (bindat-pack type value))
           (header (bindat-pack
                    openrgb-header-type
                    `((dev-idx . ,(or device 0))
                      (id . ,id)
                      (size . ,(length packed)))))
           (payload (string-join (list header packed) "")))
      (process-send-string proc payload))))

;; process buffer state
(defvar-local openrgb--log-marker nil)
(defvar-local openrgb--queue-start-marker nil)
(defvar-local openrgb--current-proto-version nil)
(defvar-local openrgb--process-connect-options nil)

(defvar-local openrgb--saved-bindings-alist nil)
(defun openrgb--restore-saved-bindings ()
  "Restore bindings saved in `openrgb--saved-bindings-alist'."
  (while openrgb--saved-bindings-alist
    (let ((pair (pop openrgb--saved-bindings-alist)))
      (set (car pair) (cdr pair)))))

(defvar-local openrgb-packet-callbacks nil
  "List of callbacks to call with PACKET when a packet is enqueued.")
(defvar-local openrgb-packet-handled nil
  "Set to non-nil if the packet in `openrgb-packet-callbacks' has been handled.")

(defun openrgb-current-proto-ver ()
  "Get the current protocol version."
  openrgb--current-proto-version)

(defun openrgb-process-p (proc)
  "Return non-nil of PROC is an OpenRGB connection."
  (process-get proc 'openrgb))

(defun openrgb--proc-log (proc level fmt &rest args)
  "Log a message in PROC's buffer, at LEVEL, as if by `(format FMT &rest ARGS)'."
  (with-current-buffer
      (if proc
          (process-buffer proc)
        (current-buffer))
    (when (openrgb-can-log level)
      (let ((inhibit-read-only t)
            (level-name
             (symbol-name
              (car
               (or (--find
                    (or (eql (cdr it) level)
                        (eql (car it) level))
                    openrgb-logging-levels-alist)
                   '("unknown"))))))
        (goto-char openrgb--log-marker)
        (insert
         (format
          "[%s] (%s): %s\n"
          (format-time-string "%F %T" (current-time))
          level-name
          (string-trim (apply #'format fmt args))))))))

(defun openrgb--enqueue-packet (packet)
  "Enqueue PACKET in the current buffer, signal waiters."
  (let ((openrgb-packet-handled nil))
    (dolist (fun openrgb-packet-callbacks)
      (funcall fun packet))
    (unless openrgb-packet-handled
      (openrgb-log nil 'error "Unhandled packet, id: %s" (alist-get 'id packet)))))

(defun openrgb--pop-packet (proc pred &rest args)
  "Await a packet from PROC that satisfies PRED from `openrgb--packet-queue'.

PRED is applied to the packet followed by ARGS.

This must be called before the packet could possibly be enqueued,
otherwise it may be dropped.

Returns an appropriate `promise'."
  (promise-new
   (lambda (resolve _reject)
     (openrgb-with-process-buffer proc
       (letrec ((callback
                 (lambda (packet)
                   (when (apply pred packet args)
                     (setq openrgb-packet-handled t)
                     (funcall resolve packet)
                     (setq openrgb-packet-callbacks
                           (delq callback openrgb-packet-callbacks))))))
         (push callback openrgb-packet-callbacks))))))

(defun openrgb--try-enqueue-packet ()
  "Try to enqueue a packet in the current buffer, if any are available.

`inhibit-read-only' should probably be t when this is called."
  (goto-char openrgb--queue-start-marker)
  ;; ignore anything that doesn't start with ORGB
  (while (and
          (<= 4 (- (point-max) (point)))
          (not (looking-at "ORGB")))
    (delete-char 1))
  ;; try to read a packet
  (let ((bytes-rem (- (point-max) (point)))
        (header-end (+ (point) openrgb-header-size))
        packet-end bytes header size payload packet)
    (when (<= openrgb-header-size bytes-rem) ;; header available
      (setq
       bytes (buffer-substring-no-properties (point) header-end)
       header (bindat-unpack openrgb-header-type bytes)
       size (alist-get 'size header))
      (when (<= size (- bytes-rem openrgb-header-size)) ;; whole packet available
        (setq
         packet-end (+ header-end size)
         payload (buffer-substring-no-properties header-end packet-end)
         packet `((payload . ,payload) ,@header))
        (delete-region (point) packet-end) ;; yum
        (openrgb-log nil 'trace "Packet received: %s" packet)
        (openrgb--enqueue-packet packet)
        t))))

(defun openrgb--process-filter (proc out)
  "Process filter for an OpenRGB process PROC, receiving OUT.

See `set-process-filter'."
  (cl-assert (openrgb-process-p proc))
  (openrgb-with-process-buffer proc
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert out)
      (while (openrgb--try-enqueue-packet)))))

(defun openrgb--process-sentinel (proc change)
  "Sentinel for an OpenRGB process PROC, receiving CHANGE.

See `set-process-sentinel'."
  (cl-assert (openrgb-process-p proc))
  (openrgb-with-process-buffer proc
    (openrgb--restore-saved-bindings)
    (condition-case err
        (cond
         ((equal change "open\n")
          (run-hooks 'openrgb-post-connect-hooks))
         ((equal change "failed\n")
          (run-hooks 'openrgb-failed-connect-hooks)))
      (error (openrgb-log proc 'error "%s" (error-message-string err)))))
  (openrgb-log proc 'info "%s" change))

(defun openrgb--make-network-process (buf host port)
  (with-current-buffer buf
    (setq openrgb--current-proto-version 0)
    (setq openrgb--process-connect-options (list host port))
    (make-network-process
     :name "openrgb"
     :buffer buf
     :host (or host 'local)
     :service (or port 6742)
     :coding 'no-conversion
     :filter #'openrgb--process-filter
     :sentinel #'openrgb--process-sentinel
     :plist '(openrgb t)
     :nowait t)))

(defun openrgb-reconnect (proc)
  "Attempt to reconnect an OpenRGB process PROC.

Returns PROC if it's still alive, nil if the buffer has been
killed, or the new process otherwise."
  (cl-assert (openrgb-process-p proc))
  (if (process-live-p proc)
      proc
    (let ((buf (process-buffer proc)))
      (if (buffer-live-p buf)
          (apply
           #'openrgb--make-network-process
           buf
           (buffer-local-value 'openrgb--process-connect-options buf))
        nil))))

(defun openrgb-connect (&optional host port)
  "Open an OpenRGB connection to HOST:PORT.

The process object is returned immediately, without waiting for
it to connect.

This captures `openrgb-post-connect-hooks',
`openrgb-failed-connect-hooks' and `openrgb-packet-callbacks' in
the lexical scope in which this was called, and restores them
when the process connects.  They can be modified later from the
process' buffer."
  (let ((buf (generate-new-buffer "*openrgb*"))
        proc)
    (with-current-buffer buf
      (set-buffer-file-coding-system 'no-conversion)
      (set-buffer-multibyte nil)

      (setq
       openrgb--saved-bindings-alist
       `((openrgb-packet-callbacks . ,openrgb-packet-callbacks)
         (openrgb-post-connect-hooks . ,openrgb-post-connect-hooks)
         (openrgb-failed-connect-hooks . ,openrgb-failed-connect-hooks))
       openrgb--log-marker (make-marker)
       openrgb--queue-start-marker (make-marker))
      (insert "=== OpenRGB Log ===\n")
      ;; advance when inserted at
      (set-marker-insertion-type openrgb--log-marker t)
      (insert "=")
      (set-marker openrgb--log-marker (1- (point)) buf)
      (insert "== Buffered Data ===\n")
      ;; stay when inserted at
      (set-marker-insertion-type openrgb--queue-start-marker nil)
      (set-marker openrgb--queue-start-marker (point) buf)
      (setq buffer-read-only t))

    (setq proc (openrgb--make-network-process buf host port))

    proc))

(provide 'openrgb-proc)
;;; openrgb-proc.el ends here
