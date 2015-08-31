;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;;; cl-challenges.asd

(in-package :cl-user)

(asdf:defsystem #:cl-challenges
  :name "cl-challenges"
  :version "0.1"
  :description "Solved challenges with explanations in Common Lisp"
  :author "Zlatozar Zhelyazkov <zlatozar@gmail.com>"

  :serial t
  :components ((:file "packages")
               (:file "tools/test")
               (:file "cl-aliases")

               (:file "algorithms/utopian_tree")
               (:file "trees/lowest_common_ancestor")

               ))
