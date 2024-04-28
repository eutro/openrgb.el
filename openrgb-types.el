;;; openrgb-types.el --- Type definitions and utilities for OpenRGB -*- lexical-binding: t -*-

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

;; This file defines some `bindat' types and utilities for dealing with them.

;;; Code:

(require 'openrgb-core)
(eval-when-compile
  (require 'openrgb-macs))
(require 'bindat)
(require 'cl-lib)
(require 'dash)
(require 's)

(eval-and-compile
  (defun openrgb-describe-bindat-type (body &optional plural)
    "Return a string definition for BODY of a `bindat' form.

Return a pluralised string if PLURAL is non-nil."
    (let (type args)
      (if (or (keywordp (car body)) (consp (car body)))
          (setq type 'struct args body)
        (setq type (car body) args (cdr body)))
      (cl-case type
        (uint (format
               (if plural
                   "unsigned %s bit integers"
                 "an unsigned %s bit integer")
               (car args)))
        (sint (format
               (if plural
                   "signed %s bit integers"
                 "a signed %s bit integer")
               (car args)))
        (vec (format
              (if plural
                  "vectors of %s"
                "a vector of %s")
              (openrgb-describe-bindat-type (cdr args) t)))
        ((str strz openrgb-string) (if plural "strings" "a string"))
        (openrgb-seq (openrgb-describe-bindat-type `(,(car args) 0 ,@(cdr args)) plural))
        (openrgb-prefix-length (openrgb-describe-bindat-type (car args) plural))
        (openrgb-omit-keys
         (openrgb-describe-bindat-type
          (--remove
           (memq (car it) (car args))
           (cdr args))
          plural))
        (openrgb-version
         (format "%s (only protocol version %s and up)"
                 (openrgb-describe-bindat-type (cadr args) plural)
                 (car args)))
        (unit (format "%S" (car args)))
        (if (format "%s if `%S',\notherwise %s"
                    (openrgb-describe-bindat-type (cadr args) plural)
                    (car args)
                    (openrgb-describe-bindat-type (caddr args) plural)))
        (struct
         (if (memq :unpack-val args)
             "unknown"
           (let* ((fields
                   (let (last-was-kw)
                     (--filter
                      (cond
                       (last-was-kw (setq last-was-kw nil) nil)
                       ((keywordp it) (setq last-was-kw t) nil)
                       ((consp it) t)
                       (t nil))
                      args)))
                  (field-descs
                   (--map
                    (-let* (((name . type) it)
                            ;; NB: we don't care about extra arguments, just pass them on
                            (type-desc (openrgb-describe-bindat-type type))
                            (type-desc-lines (s-split "\n" type-desc)))
                      ;; indent subsequent lines
                      (setcdr type-desc-lines
                              (--map
                               (format "    %s" it)
                               (cdr type-desc-lines)))
                      (format "- `%s': %s" name (string-join type-desc-lines "\n")))
                    fields)))
             (format
              "an alist with keys:\n%s"
              (string-join field-descs "\n")))))
        (otherwise
         (or
          (-when-let* ((description-sym
                        (and (string-prefix-p "openrgb-" (symbol-name type))
                             (intern (format "%s-description" type))))
                       ((singular-desc plural-desc long-desc)
                        (and (boundp description-sym)
                             (symbol-value description-sym))))
            (format "%s, see `%s'"
                    (if plural plural-desc singular-desc)
                    (intern (format "%s-type" type))))
          (error "Type %s cannot be described" type)))))))

(eval-when-compile
  (defmacro define-openrgb-type (name singular-desc plural-desc &rest body)
    "Define a `bindat' type NAME.

SINGULAR-DESC and PLURAL-DESC are the short descriptions that
`openrgb-describe-bindat-type' should use.

BODY is the list of arguments to pass directly to `bindat-type'.

This will define two constants `NAME-type' and
`NAME-description', containing a `bindat' type spec and string
description, respectively."
    (declare (indent 1) (doc-string 2))
    (cl-assert (and (symbolp name)
                    (stringp singular-desc)
                    (stringp plural-desc)))
    (let* ((typesym (intern (format "%s-type" name)))
           (descsym (intern (format "%s-description" name)))
           (description (openrgb-describe-bindat-type body)))
      `(progn
         (eval-and-compile
           (defconst ,descsym
             (list
              ,singular-desc
              ,plural-desc
              ,description)
             ,(format "Descriptions for `%s'." typesym)))
         (openrgb-expand
          (defconst ,typesym
            (bindat-type ,@body)
            ,(if (equal description "unknown")
                 (format "%s." singular-desc)
               (format "Bindat type-spec representing %s.\n\nThe type representation is %s%s"
                       singular-desc
                       description
                       (if (s-contains? "\n" description)
                           ""
                         "."))))
          (bindat-defmacro ,name ()
            ,(format "Bindat macro for `%s'." typesym)
            '(type ,typesym)))))))

(bindat-defmacro openrgb-version (proto-ver expr &optional dflt)
  "Check against PROTO-VER and return EXPR if available, DFLT otherwise."
  `(if (<= ,proto-ver (openrgb-current-proto-ver))
       ,expr
     (unit ,dflt)))

(bindat-defmacro openrgb-prefix-length (type)
  "Prefix TYPE with the total encoded length in bytes, as a u32.

This includes the size of the prefixed length, so is 4 greater
than the encoded length of TYPE.

TYPE must not capture any local variables, that is, it must be
definable at the top-level."
  (let ((bindat-type-expr
         (openrgb-maybe-pre-emit-constant
          `(bindat-type ,@type))))
    `(struct
      :pack-var value
      (_ uint 32 t :pack-val (+ 4 (bindat-length ,bindat-type-expr value)))
      (payload ,@type :pack-val value)
      :unpack-val payload)))

(bindat-defmacro openrgb-omit-keys (keys &rest type)
  "TYPE, but with KEYS removed from the alist."
  (let ((v (gensym "v")))
    `(struct
      (,v ,@type)
      :unpack-val (--remove (memq (car it) ',keys) ,v))))

(defconst openrgb-header-size 16)
(define-openrgb-type openrgb-header
  "an OpenRGB packet header" "OpenRGB packet headers"
  (magic str 4 :pack-val "ORGB")
  (dev-idx uint 32 t)
  (id uint 32 t)
  (size uint 32 t))

(bindat-defmacro openrgb-seq (kind &rest args)
  "A u16-length prefixed sequence of `(KIND <length> ARGS)'."
  (let ((v (gensym "v"))
        (len (gensym "len"))
        (val (gensym "val")))
    `(struct
      :pack-var ,v
      (,len uint 16 t :pack-val (length ,v))
      (,val ,kind ,len ,@args :pack-val ,v)
      :unpack-val ,val)))

(define-openrgb-type openrgb-color
  "a 4-byte colour value" "4-byte colour values"
  uint 32 t)

(defun openrgb--string-strip-nullterm (s)
  "Remove the last character of S if it is null."
  (if (string-suffix-p "\0" s)
      (substring s 0 (1- (length s)))
    s))
(define-openrgb-type openrgb-string
  "a null-terminated string prefixed with its u16 length." "strings"
  :pack-var v
  (val openrgb-seq str :pack-val (s-append "\0" v))
  :unpack-val (openrgb--string-strip-nullterm val))

(define-openrgb-type openrgb-mode
  "an OpenRGB mode struct" "OpenRGB mode structs"
  (name openrgb-string)
  (value sint 32 t)
  (flags uint 32 t)
  (speed-min uint 32 t) (speed-max uint 32 t)
  (brightness-min . (openrgb-version 3 (uint 32 t)))
  (brightness-max . (openrgb-version 3 (uint 32 t)))
  (colors-min uint 32 t) (colors-max uint 32 t)
  (speed uint 32 t)
  (brightness . (openrgb-version 3 (uint 32 t)))
  (direction uint 32 t)
  (color-mode uint 32 t)
  (colors openrgb-seq vec openrgb-color))

(define-openrgb-type openrgb-zone
  "an OpenRGB zone struct" "OpenRGB zone structs"
  (name openrgb-string)
  (type sint 32 t)
  (leds-min uint 32 t) (leds-max uint 32 t)
  (leds-count uint 32 t)
  (matrix-len uint 16 t)
  (matrix-height . (if (> matrix-len 0) (uint 32 t) (unit nil)))
  (matrix-width . (if (> matrix-len 0) (uint 32 t) (unit nil)))
  (matrix-data . (if (> matrix-len 0)
                     (vec (/ (- matrix-len 8) 4) . (sint 32 t))
                   (unit nil))))

(define-openrgb-type openrgb-led
  "an OpenRGB LED struct" "OpenRGB LEDs"
  (name openrgb-string)
  (value uint 32 t))

(provide 'openrgb-types)
;;; openrgb-types.el ends here
