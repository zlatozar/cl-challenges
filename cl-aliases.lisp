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
(alias 'endp 'empty?)
(alias 'evenp 'even?)
(alias 'oddp 'odd?)
(alias 'integerp 'integer?)
(alias 'minusp 'negative?)
(alias 'functionp 'function?)
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
;;;                                                      Destructive operations

(alias 'nreverse 'reverse!)
(alias 'delete 'delete!)
(alias 'nconc 'conc!)

;;; ____________________________________________________________________________
;;;                                                                Better names

(alias 'char 'char-at)

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

(alias 'reduce 'accumulate)
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

(defmacro repeat (number &body expressions)
  `(let ((n ,number)
         (thunk #'(lambda () ,@expressions)))
     (loop (if (<= n 0) (RETURN))
        (funcall thunk)
        (setq n (- n 1)))))

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

(defun group-by (pred list)
  (loop
     while list
     for cur = (pop list)
     collect
       (nreverse
        (loop with acc = (list cur)
           while list
           for x = (pop list)
           if (funcall pred cur x)
           do (push x acc)
           else do (progn (push x list)
                          (return acc))
           finally (return acc)))))

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

;; Using `iterate' package. Nice naming convention by the way.
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

(defun subsets-of-size-k (l k)
  (labels ((ss (len n little-acc big-acc)
             (cond ((zerop n) (cons little-acc big-acc))
                   ((> n len) big-acc)
                   (t
                    (let ((last-pair (nthcdr (- len 1) l)))
                      (ss (- len 1) (- n 1)
                          (if (eq (cdr last-pair) little-acc)
                              last-pair
                              (cons (car last-pair) little-acc))
                          (ss (- len 1) n little-acc big-acc)))))))
    (ss (length l) k '() '())))

(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

(defun firstn (lst n)
  (if (or (null lst) (<= n 0))
      nil
      (cons (car lst)
            (firstn (cdr lst) (- n 1)))))

(defun butlastn (seq n)
  (subseq seq 0 (- (length seq) n)))

;;; ____________________________________________________________________________
;;;                                                                    Controls

(defun ifnot (bad val)
  (unless (eql bad val) val))

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

(defmacro if-let (binding body &rest false-body)
  (let* ((binds (first binding))
         (pred (first  binds))
         (init (second binds)))
    `(let ((,pred ,init))
       (if ,pred ,body ,@false-body))))

(defmacro when-let (binding body)
  (let* ((binds (first binding))
         (pred (first  binds))
         (init (second binds)))
    `(let ((,pred ,init))
       (when ,pred ,body))))

;;; ____________________________________________________________________________
;;;                                                                     Strings

;; Comment code, Emacs will highlight it.
(defmacro comment (&rest xs))

(defun make-keyword (thing)
  (values (intern (string-upcase thing) :keyword)))

(defun stringify (x)
  (format nil "~a" x))

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

(defun join (delimiter strings)
  (let ((template (format nil "~~{~~a~~^~a~~}" delimiter)))
    (format nil template strings)))

(defun list-join (delimiter list)
  (loop for (element . more) on list
     collect element
     when more
     collect delimiter))

(defun splice (list &key (start 0) (end (length list)) new)
  "Replace a sub-sequence of an array with some other sequence
 not necessarily of the same length."
  (setf list (cons nil list))
  (let ((reroute-start (nthcdr start list)))
    (setf (cdr reroute-start)
          (nconc (make-list (length new))
                 (nthcdr (- end start)
                         (cdr reroute-start)))
          list (cdr list)))
  (replace list new :start1 start)
  list)

;;; ____________________________________________________________________________
;;;                                                                   Utilities

(defmacro zap! (fn place &rest args)
  "Evaluating an expression involving a variable and assigning the result of that
expression to the variable at the same time"
  (multiple-value-bind
        (temps exprs stores store-expr access-expr)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores)
             (funcall ,fn ,access-expr ,@args)))
       ,store-expr)))

(defun print-hash (hash-table)
  "Print the hash table as: Key, Value~% "
  (loop for k being the hash-keys in hash-table
     do (format t "~A, ~A~%" k (gethash k hash-table))))

(defmacro defconst (name value &optional (documentation nil docp))
  (let ((global (intern (format nil "%%~A" (symbol-name name)))))
    `(progn
       (defvar ,global ,value ,@(and docp `(,documentation)))
       (define-symbol-macro ,name (load-time-value ,global t)))))
