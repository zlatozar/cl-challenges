;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package #:cl-user)

(defmacro alias (cl-name new-name)
  `(setf (symbol-function ,new-name) (symbol-function ,cl-name)))

;; Predicates
(alias 'null 'null?)

(alias 'eq 'eq?)
(alias 'eql 'eql?)
(alias 'equalp 'equal?)

(alias 'listp 'list?)
(alias 'evenp 'even?)
(alias 'oddp 'odd?)
(alias 'integerp 'integer?)
(alias 'minusp 'negative?)
(alias 'functionp 'procedure?)
(alias 'consp 'cons?)
(alias 'atom 'atom?)
(alias 'characterp 'char?)
(alias 'stringp 'string?)
(alias 'numberp 'number?)
(alias 'zerop 'zero?)

;; Higher order functions
(alias 'mapc 'for-each)
(alias 'remove-if-not 'filter)

;; Better names
(alias 'char 'char-at)
(alias 'length 'string-length)

;; Weird names
(alias 'princ 'display)
(alias 'terpri 'newline)
(alias 'read-line 'read-string)
(alias 'nthcdr 'drop)
(alias 'subseq 'substring)
(alias 'rem 'reminder)
(alias 'listen 'char-ready?)
(alias 'ash 'arithmetic-shift)

;; Not existing
(defun range (max &key (min 0) (step 1))
  (loop for n from min below max by step
     collect n))
