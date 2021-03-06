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
        #:pcl-test))

(defpackage #:trees
  (:documentation "Tree algorithms")
  (:use #:common-lisp
        #:iterate
        #:pcl-test)

  (:import-from :paip
                #:find-anywhere)

  (:export #:lca))

;;; ____________________________________
;;;                             Iterate

(defpackage #:learn-iter
  (:documentation "Do not LOOP - Iterate")
  (:use #:common-lisp
        #:iterate))
