;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;;; cl-aliases.lisp: Inspired by Clojure and Scheme

(in-package #:cl-user)

(defmacro alias (cl-name new-name)
  `(setf (symbol-function ,new-name) (symbol-function ,cl-name)))

;;; ____________________________________________________________________________
;;;                                                                  Predicates

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

(defun single? (lst)
  (and (consp lst) (not (cdr lst))))

(defun gensym? (x)
  "Is X a gensym'd (uninterned) symbol?"
  (and (symbolp x) (not (symbol-package x))))

;;; ____________________________________________________________________________
;;;                                                                Better names

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

;;; ____________________________________________________________________________
;;;                                                                   Sequences

(alias 'mapc 'for-each)

(defun range (max &key (min 0) (step 1))
  (loop for n from min below max by step
     collect n))

(defmacro for (var start stop &rest body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defun foldl (function initval the-list)
  "Wrapper for (REDUCE <...> :INITIAL-VALUE <...> :FROM-END NIL) for
   the sake of a more familiar and terse syntax."
  (reduce function the-list :initial-value initval))

(defun foldr (function initval the-list)
  "Wrapper for (REDUCE <...> :INITIAL-VALUE <...> :FROM-END T) for
   the sake of a more familiar and terse syntax."
  (reduce function the-list :initial-value initval :from-end t))

(defun filter (predicate sequence &rest args)
  "Wrapper for (REMOVE-IF-NOT <...>) the sake of
   a more familiar and terse syntax"
  (apply #'remove-if-not predicate sequence args))

(defun flip (f)
  (lambda (&rest args)
    (apply f (nreverse args))))

(defun split-at (list k)
  (declare (list list))
  (loop for i below k
     for (x . right) on list
     collect x into left
     finally (return (values left right))))

(defmacro -> (x &rest forms)
  (flet ((expand-form (x form)
           (cond
             ((atom form)
              `(,form ,x))
             ((member (car form) '(lambda function))
              `(funcall ,form ,x))
             (T `(,(car form) ,x ,@(cdr form))))))
    (loop
       for form in forms
       for y = (expand-form x form)
       then (expand-form y form)
       finally (return y))))

;; Using `iterate' package
;;
;; (defmacro -> (x &rest forms)
;;   (iter
;;     (for form :in forms)
;;     (for y :=
;;          (case (car (when (consp form) form))
;;            ((lambda function) `(funcall ,form ,x))
;;            ('nil `(,form ,x))
;;            (otherwise `(,(car form) ,x ,@(cdr form)))))
;;     (setf x y)
;;     (finally (return y))))

;;; ____________________________________________________________________________
;;;                                                                    Controls

(defmacro nor (&rest forms)
  "Equivalent to (not (or ...))."
  (if (null forms) t
      (if (null (rest forms))
          `(not ,(first forms))
          `(if ,(first forms)
               nil
               (nor ,@(rest forms))))))

(defmacro nand (&rest forms)
  "Equivalent to (not (and ...))."
  (if (null forms) nil
      (if (null (rest forms))
          `(not ,(first forms))
          `(if ,(first forms)
               (nand ,@(rest forms))
               t))))

;;; ____________________________________________________________________________
;;;                                                                     Strings

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun space-char? (c)
  "Tests to see if character is white space"
  (or (char= #\space c)
      (char= #\tab c)
      (char= #\newline c)
      (char= #\return c)))

(defun string->list (str)
  (coerce str 'list))

(defun list->string (list)
  (coerce list 'string))
