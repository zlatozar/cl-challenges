;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;;; package.lisp: Define Lisp Challenges packages.
;;;;               External libraries: ITERATE and PAIP.

(in-package :cl-user)

;;; ____________________________________
;;;                          Algorithms

(defpackage #:algo
  (:documentation "Just algorithms")
  (:use #:common-lisp
        #:iterate
        #:paip))

(defpackage #:trees
  (:documentation "Tree algorithms")
  (:use #:common-lisp
        #:iterate
        #:paip))

;;; ____________________________________
;;;                             Iterate

(defpackage #:learn-iter
  (:documentation "Do not LOOP - Iterate")
  (:use #:common-lisp
        #:iterate
        #:paip))
