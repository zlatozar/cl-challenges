;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ALGO; Base: 10 -*-

(in-package #:trees)

;; VMWare interview

;; Given a binary tree, find the lowest common ancestor of two given nodes in the tree.

;;; ____________________________________________________________________________




;;; TEST




;;; ____________________________________________________________________________
;;;                                                                    My notes

;; It is strange that representation is not given so the first question is:
;; How to represent a tree in Common Lisp?
;;
;; The easiest way is as nested list.
;; For example: (1 (2 (6 7)) (3) (4 (9 (12))) (5 (10 11)))
;;
;; The first element of a list is the root of that tree (or a node) and the rest of the
;; elements are the subtrees (every new open parenthesis is a next level of the tree).
;; For example, the subtree (6 7) is a tree with 2 at its root and the elements 6 7
;; as its children. So I start digging on this.
;;
;; Another representation that I found is to use Paul Graham "ANSI Common Lisp"
;; representation:

(defstruct node
  elt (l nil) (r nil))

;; Because I have binary tree may be a better way to represent it with nested lists is to
;; use a-list (more readable). For example:
;; (1 (2 (6 . 7)) (3) (4 (9 (12))) (5 (10 . 11))). Even better - display empty.
;; (0 (1 (2 . 3) NIL) (4 (5 (6) NIL) NIL))
;;
;; What is a node? (elm (left right))
;;
;; This is not consistent - (left right) could not be treated as 'elm'. Let's fix it:
;; (0 (1 (2) (3)) (4 (5 (6))))
;;
;; Great now I have the tree!

(defparameter *bin-tree* '(0 (1 (2) (3)) (4 (5 (6)))))

;;; Make tree

(defun make-bin-leaf (elm)
  "Create a leaf."
  (list elm))

(defun make-bin-node (parent elm1 &optional elm2)
  (list parent elm1 elm2))

;; test creation
(deftest test-make-bin-tree ()
  (check
    (equal (make-bin-node 0 (make-bin-node 1 (make-bin-leaf 2)
                                           (make-bin-leaf 3))
                          (make-bin-node 4 (make-bin-node 5 (make-bin-leaf 6))))
           '(0 (1 (2) (3)) (4 (5 (6) NIL) NIL)) )))

(defun node-elm (node)
  (first node))

(defun node-left (node)
  (first (second node)))

(defun node-right (node)
  (first (third node)))

;;; Predicates

(defun leaf-p (node)
  "Test if binary tree NODE is a leaf."
  (and (listp node)
       (= (list-length node) 1)))

(defun node-p (node)
  "Test if binary tree node is a node."
  (and (listp node)
       (= (list-length node) 3)))

(deftest test-find-anywhere ()
  (check
    (eql (find-anywhere 3 *bin-tree*) 3)))

(defun member-p (elm tree)
  (eql (find-anywhere elm tree) elm))

;;; Search

;; It is important to understand that goal and successors functions should be fast
;; and if possible - easy to be written.
;;
;; If tree is represented as a list there is no way to find out successors if you pass
;; just element (number in our case). That's why node should be passed and we should take
;; care for that as passing tree not just element:
;;
;; (depth-first-search *bin-tree* goal-p successors-fn)

;; Defines legal next moves for a given node
(defun successors-fn (node)
  (rest node))

(defun is= (value)
  #'(lambda (x) (eql (car x) value)))

;; NOTE: (lambda (x) ...)
;;       '(lambda (x) ...)
;;       #'(lambda (x) ...) This one is used in book.
;;
;; LAMBDA is a macro. It expands to (function (lambda ...)), which is the equivalent of
;; #'(lambda ...). It allows to easily identify actual functions used as values. It also
;; makes it easier to replace with an flet or labels function.

(defun depth-first-search (tree goal-p successors-fn)
  (tree-search (list tree) goal-p successors-fn #'append))

;; Test DFS for a given tree
(deftest test-depth-first-search ()
  (check
    (equal (depth-first-search *bin-tree* (is= 1)  #'successors-fn)
           '(1 (2) (3)) )))

;; From where to start? As a rule I start from easiest case - if nodes have as a
;; common parent the root node of the binary tree.
;;
;; Algorithm will be:
;; - find first in a left sub-tree
;; - find second in a right sub-tree
