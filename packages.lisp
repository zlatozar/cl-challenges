;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ALGO; Base: 10 -*-

;;;; package.lisp: Define Lisp Challenges packages. External libraries: iterate

(in-package :cl-user)

;;; ____________________________________
;;;                               Tools

(defpackage #:inspect
  (:documentation "Package contains functions that are
collected from other places and could help during coding sessions.")
  (:use #:common-lisp)
  (:export #:??
           #:?a
           #:?p~
           #:?p+
           #:?p*
           #:?p%
           #:?mac))

(defpackage #:pcl-test
  (:documentation "Use defined in the book 'Practical Common Lisp'
test framework to test chapter exercises.")
  (:use #:common-lisp)
  (:export #:deftest
           #:check))

;;; ____________________________________
;;;                    Helper functions

(defpackage #:aux
  (:documentation "Useful functions taken from Lisp books.")
  (:use #:common-lisp)
  (:export #:declare-ignore
           #:true
           #:false
           #:nothing
           #:not-null
           #:is
           #:dbg
           #:dbg-indent
           #:debugit
           #:find-all
           #:find-all-if
           #:mappend
           #:mklist
           #:flatten
           #:random-elt
           #:compose
           #:range
           #:defun-memo
           #:delay
           #:force
           #:find-anywhere
           #:find-if-anywhere
           #:tree-search
           #:make-queue
           #:enqueue
           #:dequeue))

;;; ____________________________________
;;;                          Algorithms

(defpackage #:algo
  (:documentation "Algorithm challenges")
  (:use #:common-lisp
        #:inspect
        #:pcl-test
        #:iterate
        #:aux))
