;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ALGO; Base: 10 -*-

(in-package #:algo)

;; HackerRank

;; Given an array of integers, every element appears twice except for one. Find that
;; single one.

;;; ____________________________________________________________________________





;;; TEST




;;; ____________________________________________________________________________
;;;                                                                    My notes

;; First let's pretend that array is sorted - simplify the problem.
;; For example for array (2 2 1 1 4 4 5) we have to use reduce that eliminate two equals.
;; That is the key point to use XOR even array is not sorted because XOR is commutative.
;; (Binary operation is commutative if changing the order of the operands does not change
;; the result)
;;
;; ((2^2)^(1^1)^(4^4)^(5)) => (0^0^0^5) => 5.
;; Hence picking the odd one out (5 in this case).
;;
;; Let's now generalize the problem: "Every element appears n-times except for one.
;; Find that single one."
;;
;; Procedure
;; ---------
;; 1) Convert each number to base-n. Let's say the longest number has k digits (in base-n)
;; 2) For all k digits, sum up all of the digits in the kth slot, and then take the result
;; modulo n. The resulting digits make up the element that only occurs once.
;;
;; XOR use base-2 it is the same procedure but for base-n
