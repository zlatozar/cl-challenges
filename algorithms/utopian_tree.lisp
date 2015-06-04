;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ALGO; Base: 10 -*-

(in-package #:algo)

;; HackerRank

;; The Utopian tree goes through 2 cycles of growth every year. The first growth cycle of the
;; tree is during the monsoon season when it doubles in height. The second growth cycle is
;; during the summer when it increases in height by 1 meter. If a new Utopian tree sapling of
;; height 1 meter is planted just before the onset of the monsoon season, can you find the
;; height of the tree after N cycles?

;; Sample Input: 2 3 4

;; Sample Output: 6 7

;; There are 2 testcases. When N = 3, the height of the tree at the end of the 1st cycle
;; doubles to 2, at the end of the 2nd cycle it increases by 1 and at the end of the 3rd
;; cycle, it doubles its height to 6. When N = 4, the height of the tree increases from 6 to
;; 7.

;;; ____________________________________________________________________________

(defun utopain-tree (test-case)
  (let ((declared-tests (first test-case))
        (test-cycles (rest test-case)))

    (when (/= declared-tests (length test-cycles))
      (cerror "Continue and run for passed cycles."
              "Declared ~D test cases - passed ~D cycles."
              declared-tests (length test-cycles)))

    (mapcar #'length-tree test-cycles)))

(defun length-tree (cycles &optional (year -1) (len-so-far 0))
  "Find the height of the tree after given `cycles'"
  (if (= cycles year)
      len-so-far
      (length-tree cycles (incf year)
                   (if (evenp year)
                       (+ 1 len-so-far)
                       (* 2 len-so-far)))))

;;; TEST

(deftest test-utopian-tree ()
  (check
    (equal (utopain-tree '(1 0)) '(1))
    (equal (utopain-tree '(2 0 1)) '(1 2))
    (equal (utopain-tree '(2 2 3)) '(3 6))
    (equal (utopain-tree '(2 4 5)) '(7 14))
    (equal (utopain-tree '(2 6 7)) '(15 30))
    (equal (utopain-tree '(2 10 20)) '(63 2047))))

;;; ____________________________________________________________________________
;;;                                                                    My notes

;; I have to say that challaenge definition is quite hairy and why should specify number
;; of tests first?! So first I strugle with it. Could be reduced to "If year is even add
;; one else double it". That is easy, so I decide to complicate the task: "Write tail
;; recursive program for Utopian tree challenge".
