;;; openrgb-constants.el --- Enum definitions for OpenRGB -*- lexical-binding: t -*-

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

;; This file includes the definitions of some OpenRGB enumerations
;; that exist in its C++ code.

;;; Code:

(require 'dash)
(eval-when-compile
  (require 'cl-macs))

(eval-when-compile
  (defmacro define-openrgb-enum (name doc &rest symbols)
    "Define NAME as a C-like enumeration of SYMBOLS.

This will define two functions `NAME-to-int' and `NAME-from-int',
which perform the conversions to/from symbols."
    (declare (indent 1) (doc-string 2))
    (let* ((enum-to-int (intern (format "%s-to-int" name)))
           (enum-from-int (intern (format "%s-from-int" name)))
           (valid-symbols-doc (format "See `%s' for the list of symbols." name)))
      `(progn
         (defconst ,name ',symbols
           ,(format "%s

These enum constants can be converted to and from integers using
`%s'and `%s'."
                    doc enum-to-int enum-from-int))
         (defun ,enum-to-int (symbol)
           ,(format "Convert SYMBOL from %s to an integer.

Returns nil if it is not a valid symbol.

%s"
                    name valid-symbols-doc)
           (--some (when (eq it symbol) it-index) ,name))
         (defun ,enum-from-int (n)
           ,(format "Convert N to a symbol from %s.

Returns `unknown' if the value is unrecognised.

%s"
                    name valid-symbols-doc)
           (or (--find (eq it-index n) ,name) 'unknown))))))

;; constants from https://gitlab.com/CalcProgrammer1/OpenRGB/-/blob/master/RGBController/RGBController.h
;; checked at revision 42542b6b676738c793bb2a84258498c6fe96e8ac

(define-openrgb-enum openrgb-zone-types
  "OpenRGB zone type constants."
  single linear matrix)

(define-openrgb-enum openrgb-mode-direction
  "OpenRGB mode direction constants."
  left right up down horizontal vertical)

(define-openrgb-enum openrgb-mode-color-types
  "OpenRGB mode color type constants."
  none                        ; Mode has no colors
  per-led                     ; Mode has per LED colors selecte
  mode-specific               ; Mode specific colors selected
  random                      ; Mode has random colors selected
  )

(define-openrgb-enum openrgb-device-types
  "OpenRGB device type constants."
  motherboard dram gpu cooler ledstrip keyboard mouse mousemat headset headset_stand gamepad
  light speaker virtual storage case microphone accessory keypad)

(define-openrgb-enum openrgb-mode-flag-idx
  "OpenRGB mode flags by index.

These enum constants can be converted to and from bit masks using
`openrgb-mode-flags-to-int' and `openrgb-mode-flags-from-int'."
  has-speed                            ; Mode has speed parameter
  has-direction-lr                     ; Mode has left/right parameter
  has-direction-ud                     ; Mode has up/down parameter
  has-direction-hv                     ; Mode has horiz/vert parameter
  has-brightness                       ; Mode has brightness parameter
  has-per-led-color                    ; Mode has per-LED colors
  has-mode-specific-color              ; Mode has mode specific colors
  has-random-color                     ; Mode has random color option
  manual-save                          ; Mode can manually be saved
  automatic-save                       ; Mode automatically saves
  )

(defun openrgb-mode-flags-to-int (symbols)
  "Convert a list of `openrgb-mode-flag-idx' SYMBOLS to a bit-mask integer."
  (--reduce-from
   (logior
    acc
    (ash 1
         (or (openrgb-mode-flag-idx-to-int it)
             (error "Not a valid flag: %s" it))))
   0
   symbols))

(defun openrgb-mode-flags-from-int (mask)
  "Convert a bit-mask MASK to a list of `openrgb-mode-flag-idx' symbols."
  (cl-assert (<= 0 mask))
  (let ((syms openrgb-mode-flag-idx)
        ret)
    (while (and (< 0 mask) syms)
      (unless (zerop (logand mask 1))
        (push (car syms) ret))
      (setq mask (ash mask -1))
      (pop syms))
    (unless (zerop mask)
      (push 'unknown ret))
    (nreverse ret)))

(provide 'openrgb-constants)
;;; openrgb-constants.el ends here
