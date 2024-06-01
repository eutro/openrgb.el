;;; openrgb-packets.el --- Packet declarations for OpenRGB -*- lexical-binding: t -*-

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

;; This file includes the definitions of all known endpoints for
;; OpenRGB.

;;; Code:

(require 'openrgb-core)
(eval-when-compile
  (require 'openrgb-macs))
(eval-and-compile
  (require 'openrgb-types))
(require 'promise)

(defun openrgb--send-packet (proc id device name type resp-type payload)
  "Send a packet with ID and DEVICE to PROC.

Log with name NAME, TYPE is the `bindat' type of PAYLOAD,
RESP-TYPE is nil or the response type."
  (let (res)
    (when resp-type
      (setq res (openrgb--recv proc id resp-type))
      (promise-then
       res
       (lambda (val) (openrgb-log proc 'debug "%s got: %s" name val))
       (lambda (err) (openrgb-log proc 'error "%s error: %s") err)))
    (openrgb--send proc id device payload type)
    res))

(eval-when-compile
  (defmacro define-openrgb-packet (name args &rest spec)
    "Define an openrgb packet and response.

NAME is the symbol name of the packet, ARGS is a list of
arguments to use in the body.

SPEC is a plist of keywords and values:

- `:doc' documentation for the endpoint
- `:id' (required) is the packet ID
- `:device' non-nil if a device index is required
- `:type' (required) the `bindat' type of the packet's payload
- `:resp' the `bindat' type of the response
- `:value' (required) the value to encode as `:type'"
    (declare (indent 2))
    (let* ((sendsym (intern (format "%s/send-type" name)))
           (recvsym (intern (format "%s/recv-type" name)))
           (resp (plist-get spec :resp))
           (doc-fmt "%s

Sends the packet to SERVER, an `openrgb-process-p'.

%s")
           (doc (format
                 doc-fmt
                 (or (plist-get spec :doc)
                     "Undocumented.")
                 (if resp
                     (let* ((desc (openrgb-describe-bindat-type resp))
                            (is-multiline (s-contains? "\n" desc))
                            (doc
                             (format
                              "Returns a `promise-class' with the result, which is %s%s"
                              desc
                              (if is-multiline "" "."))))
                       (if is-multiline doc
                         (string-fill doc 80)))
                   "Returns nil, as there is no response from the server.")))
           (id (or (plist-get spec :id) (error "No `:id' provided")))
           (type (or (plist-get spec :type) (error "No `:type' provided")))
           (maybe-device (if (plist-get spec :device) '(device) nil))
           (value (or (plist-get spec :value) (error "No `:value' provided"))))
      `(openrgb-expand
        (defconst ,sendsym (bindat-type ,@type))
        ,(when resp `(defconst ,recvsym (bindat-type ,@resp)))
        (defun ,name (server ,@maybe-device ,@args)
          ,doc
          (cl-assert (openrgb-process-p server))
          (openrgb-with-process-buffer server
            (openrgb--send-packet
             server ,id ,@(or maybe-device '(nil))
             ',name ,sendsym ,(if resp recvsym nil)
             ,value)))))))

;; https://gitlab.com/OpenRGBDevelopers/OpenRGB-Wiki/-/raw/stable/Developer-Documentation/OpenRGB-SDK-Documentation.md
;; | Value | Name
;; | ----- | ----------------------------------
;; | 0     | REQUEST_CONTROLLER_COUNT
;; | 1     | REQUEST_CONTROLLER_DATA
;; | 40    | REQUEST_PROTOCOL_VERSION
;; | 50    | SET_CLIENT_NAME
;; | 100   | DEVICE_LIST_UPDATED
;; | 150   | REQUEST_PROFILE_LIST
;; | 151   | REQUEST_SAVE_PROFILE
;; | 152   | REQUEST_LOAD_PROFILE
;; | 153   | REQUEST_DELETE_PROFILE
;; | 1000  | RGBCONTROLLER_RESIZEZONE
;; | 1050  | RGBCONTROLLER_UPDATELEDS
;; | 1051  | RGBCONTROLLER_UPDATEZONELEDS
;; | 1052  | RGBCONTROLLER_UPDATESINGLELED
;; | 1100  | RGBCONTROLLER_SETCUSTOMMODE
;; | 1101  | RGBCONTROLLER_UPDATEMODE
;; | 1102  | RGBCONTROLLER_SAVEMODE

(define-openrgb-packet openrgb-request-controller-count ()
  :doc "Request the number of controllers."
  :id 0
  :value t
  :type (unit t)
  :resp (uint 32 t))

(define-openrgb-packet openrgb-request-controller-data ()
  :doc "Request the data of the controller with index DEVICE."
  :id 1
  :device t
  :value `((version . ,(openrgb-current-proto-ver)))
  :type ((version . (openrgb-version 1 (uint 32 t))))
  :resp (openrgb-controller-data))

(define-openrgb-packet openrgb-request-protocol-version (version)
  :doc "Request the protocol version, telling the server that ours is VERSION."
  :id 40
  :value version
  :type (uint 32 t)
  :resp (uint 32 t))

(define-openrgb-packet openrgb-set-client-name (name)
  :doc "Inform the server that we are called NAME."
  :id 50
  :value name
  :type (strz))

(define-openrgb-packet openrgb-request-profile-list ()
  :doc "Request the list of profiles from the server."
  :id 150
  :value t
  :type (unit t)
  :resp
  ;; undocumented on the wiki (as of time of writing ?)
  ;; source is here:
  ;; https://gitlab.com/CalcProgrammer1/OpenRGB/-/blob/42542b6b676738c793bb2a84258498c6fe96e8ac/ProfileManager.cpp#L482
  (openrgb-prefix-length
   (openrgb-seq vec (struct (name openrgb-string)))))

(define-openrgb-packet openrgb-request-save-profile (name)
  :doc "Save the current configuration to a profile called NAME."
  :id 151
  :value name
  :type (strz))

(define-openrgb-packet openrgb-request-load-profile (name)
  :doc "Attempt to load a profile called NAME."
  :id 152
  :value name
  :type (strz))

(define-openrgb-packet openrgb-request-delete-profile (name)
  :doc "Attempt to delete a profile called NAME."
  :id 153
  :value name
  :type (strz))

(define-openrgb-packet openrgb-rgbcontroller-resizezone (zone new-size)
  :doc "Change the size of the zone on DEVICE with index ZONE-IDX to NEW-SIZE."
  :id 1000
  :device t
  :value `((zone . ,zone) (new-size . ,new-size))
  :type ((zone sint 32 t) (new-size sint 32 t)))

(define-openrgb-packet openrgb-rgbcontroller-updateleds (colors)
  :doc "Set all the lights of DEVICE to COLORS."
  :id 1050
  :device t
  :value colors
  :type
  (openrgb-prefix-length
   (openrgb-seq vec openrgb-color)))

(define-openrgb-packet openrgb-rgbcontroller-updatezoneleds (zone colors)
  :doc "Set all the lights on DEVICE's ZONE to COLORS."
  :id 1051
  :device t
  :value `((zone . ,zone) (colors . ,colors))
  :type
  (openrgb-prefix-length
   (struct
    (zone uint 32 t)
    (colors openrgb-seq vec openrgb-color))))

(define-openrgb-packet openrgb-rgbcontroller-updatesingleled (idx color)
  :doc "Set the color on DEVICE of the IDXth light to COLOR."
  :id 1052
  :device t
  :value `((idx . ,idx) (color . ,color))
  :type ((idx uint 32 t) (color openrgb-color)))

(define-openrgb-packet openrgb-rgbcontroller-setcustommode ()
  :doc "Attempt to set the custom mode of DEVICE.

This calls `RGBController::setCustomMode' in C++.  I do not know what it does."
  :id 1100
  :device t
  :value t
  :type (unit t))

(define-openrgb-packet openrgb-rgbcontroller-updatemode (mode-idx mode-data)
  :doc "Update DEVICE\\='s MODE-IDXth mode to be MODE-DATA.

See `openrgb-mode-type' for the layout of MODE-DATA."
  :id 1101
  :device t
  :value `((mode-idx . ,mode-idx) (mode-data . ,mode-data))
  :type
  (openrgb-prefix-length
   (struct
    (mode-idx sint 32 t)
    (mode-data openrgb-mode))))

(define-openrgb-packet openrgb-rgbcontroller-savemode (mode-idx mode)
  :doc "Update and save DEVICE\\='s MODE-IDXth mode to be MODE-DATA.

See `openrgb-mode-type' for the layout of MODE-DATA."
  :id 1102
  :device t
  :value `((mode-idx . ,mode-idx) (mode . ,mode))
  :type
  (openrgb-prefix-length
   (struct
    (mode-idx sint 32 t)
    (mode openrgb-mode))))

(provide 'openrgb-packets)
;;; openrgb-packets.el ends here
