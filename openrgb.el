;;; openrgb.el --- An OpenRGB client for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024 B. Szilvasy

;; Author: B. Szilvasy <emacs-pkgs@eutro.dev>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25") (dash "2.19") (promise "1.1") (s "1.13"))
;; Keywords: comm, games
;; URL: https://github.com/eutro/openrgb.el

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

;; A low-level OpenRGB client for Emacs.
;;
;; In short:
;; - Server connections can be made with `openrgb-connect',
;;   and reconnected with `openrgb-reconnect'.
;; - Messages can be sent with the packet definitions in `openrgb-packets'.
;; - Messages that are sent from the server unprompted (and otherwise) can be
;;   received by setting `openrgb-packet-callbacks', currently this is only
;;
;; Some values sent or returned from packets are integers that are
;; enumerations or bit masks, conversions for these can be found in
;; `openrgb-constants'.

;;; Code:

(require 'openrgb-core)
(require 'openrgb-proc)
(require 'openrgb-constants)
(require 'openrgb-packets)

(defun openrgb-init-send-protocol-version ()
  "Agree on the highest supported protocol version with the server."
  (let ((proc (get-buffer-process (current-buffer))))
    (promise-then
     (openrgb-request-protocol-version
      proc openrgb-supported-proto-version)
     (lambda (ver)
       (openrgb-with-process-buffer proc
         (setq openrgb--current-proto-version
               (min openrgb-supported-proto-version ver))
         (openrgb-log
          proc 'info "Protocol version: %s"
          openrgb--current-proto-version))))))

(defcustom openrgb-client-name "openrgb.el"
  "Client name to send to the connected OpenRGB server."
  :group 'openrgb
  :type 'string)

(defun openrgb-init-send-client-name ()
  "Send `openrgb-client-name' as our client name to the server."
  (let ((proc (get-buffer-process (current-buffer))))
    (openrgb-set-client-name proc openrgb-client-name)))

(provide 'openrgb)
;;; openrgb.el ends here
