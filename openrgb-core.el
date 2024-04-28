;;; openrgb-core.el --- OpenRGB base definitions. -*- lexical-binding: t -*-

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

;; This file declares the `openrgb' group and some aliases, in order
;; to avoid dependency cycles.

;;; Code:

(defgroup openrgb nil
  "Support for OpenRGB network protocol."
  :prefix "openrgb-"
  :group 'communication)

(defcustom openrgb-post-connect-hooks
  '(openrgb-init-send-protocol-version
    openrgb-init-send-client-name)
  "Hooks to call when the OpenRGB process has connected."
  :group 'openrgb
  :type 'hook
  :options '(openrgb-init-send-protocol-version
             openrgb-init-send-client-name))

(defcustom openrgb-failed-connect-hooks '()
  "Hooks to call when the OpenRGB process fails to connect."
  :group 'openrgb
  :type 'hook)

(defconst openrgb-logging-levels-alist
  '((error . 0) (info . 1) (debug . 2) (trace . 3))
  "Symbol mapping for OpenRGB logging levels.")

;; TODO log levels
(defcustom openrgb-logging-level 1
  "Logging level to use for OpenRGB."
  :type '(choice (const :tag "Off" -1)
                 (const :tag "Error" 0)
                 (const :tag "Info" 1)
                 (const :tag "Debug" 2)
                 (const :tag "Trace" 2))
  :group 'openrgb)

(defun openrgb-can-log (level)
  "Return non-nil if LEVEL, an integer or symbol, is to be logged."
  (>= openrgb-logging-level
      (cond
       ((numberp level) level)
       ((alist-get level openrgb-logging-levels-alist))
       (t (error "No such level: %S" level)))))

(defconst openrgb-supported-proto-version 3
  "The maximum protocol version supported by this package.")

(declare-function openrgb--proc-recv "openrgb-proc")
(declare-function openrgb--proc-send "openrgb-proc")
(declare-function openrgb--proc-log "openrgb-proc")
(declare-function openrgb-current-proto-ver "openrgb-proc")
(defalias 'openrgb-log #'openrgb--proc-log)
(defalias 'openrgb--recv #'openrgb--proc-recv)
(defalias 'openrgb--send #'openrgb--proc-send)

(provide 'openrgb-core)
;;; openrgb-core.el ends here
