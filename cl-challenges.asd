;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;;; cl-challenges.asd

(in-package :cl-user)

(asdf:defsystem #:cl-challenges
  :name "cl-challenges"
  :version "0.1"
  :description "Solved challenges in Common Lisp"
  :author "Zlatozar Zhelyazkov <zlatozar@gmail.com"

  :serial t
  :components ((:file "packages")

               ;; Helper functions
               (:file "tools/introspection" :depends-on ("packages"))
               (:file "tools/test" :depends-on ("packages"))
               (:file "auxfns" :depends-on ("packages"))

               ;; Algorithms
               (:file "algo/utopian_tree")
               (:file "algo/lowest_common_ancestor" :depends-on ("auxfns"))

               ))
