;;; openrgb-macs.el --- Some utility macros for the OpenRGB package -*- lexical-binding: t -*-

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

;; This file declares a few macro utilities that other files in this package use.

;;; Code:

(eval-when-compile
  (require 'cl-macs))

(defmacro openrgb-with-process-buffer (proc &rest body)
  "In PROC's buffer, execute BODY."
  (declare (debug t) (indent 1))
  `(with-current-buffer (process-buffer ,proc)
     ,@body))

;; `openrgb-expand' and pre-emit
(defvar openrgb-expand-pre-emit-defs 'not-a-list)

(defun openrgb-can-pre-emit-def ()
  "Return non-nil if `openrgb-expand-pre-emit-defs' is a list."
  (listp openrgb-expand-pre-emit-defs))

(defun openrgb-expand-pre-emit-def (form)
  "During `openrgb-expand' expansion, emit a top-level FORM."
  (cl-assert (listp openrgb-expand-pre-emit-defs))
  (unless (member form openrgb-expand-pre-emit-defs)
    (push form openrgb-expand-pre-emit-defs)))

(defun openrgb-maybe-pre-emit-constant (form)
  "Return FORM or a pre-emitted constant variable."
  (if (openrgb-can-pre-emit-def)
      (let ((name-sym
             (intern
              (format "openrgb--constant-%s"
                      (secure-hash 'md5 (format "%S" form))))))
        (openrgb-expand-pre-emit-def
         `(defconst ,name-sym ,form))
        name-sym)
    form))

(defmacro openrgb-expand (&rest body)
  "Like `(progn &rest BODY)', but allow for `openrgb-expand-pre-emit-def'."
  (let* ((openrgb-expand-pre-emit-defs nil)
         (expr
          (macroexpand-all
           (macroexp-progn body)
           macroexpand-all-environment)))
    `(progn
       ,@(reverse openrgb-expand-pre-emit-defs)
       ,expr)))

(provide 'openrgb-macs)
;;; openrgb-macs.el ends here
