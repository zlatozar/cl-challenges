;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ALGO; Base: 10 -*-

(in-package #:trees)

;; VMWare interview

;; Given a binary tree, find the lowest common ancestor of two given nodes in the tree.

;;; ____________________________________________________________________________

(defun lca (elm p q)
  (cond ((null elm) nil)
        ((or (eq (car elm) p)
             (eq (car elm) q)) elm)
        (t (let ((left (lca-rec (cadr elm) p q))
                 (right (lca-rec (caddr elm) p q)))
             (if (and left right) (car elm)
                 (or left right))))))

;;; TEST

;; Format is ugly but more like a tree
(defparameter *bin-tree* '(0
                           (1
                            (2) (3))
                           (4
                            (5
                             (6)))))

;; Simple one
(deftest test-lca ()
  (check
    (eq (lca *bin-tree* 2 3) 1)))

;;; ____________________________________________________________________________
;;;                                                                    My notes

;; Notes style is form book "Thinking Mathematically" by J.Mason, L.Burton, K.Stacey

;; ENTER

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

;; (defstruct node
;;   elt (l nil) (r nil))

;; I usually create a lisp file and start codding, trying and testing. Sometime I receive strange errors.
;; As you know `defstruct' in Lisp also defines predicates e.g. node-p. It is possible to
;; clash names easily. Word node is quite used when we talk for trees. Doing this SBCL returns bizarre message:

;; caught WARNING:
;;   Derived type of TREE is
;;     (VALUES NODE &OPTIONAL),
;;   conflicting with its asserted type
;;     LIST.
;;   See also:
;;     The SBCL Manual, Node "Handling of Types"

;; REMEMBER
;;
;; DO NOT forget to comment unused code!

;; Because I have binary tree may be a better way to represent it with nested lists is to
;; use 'a-list' (more readable). For example:
;; (1 (2 (6 . 7)) (3) (4 (9 (12))) (5 (10 . 11))).
;;
;; Even better - display empty:
;; (0 (1 (2 . 3) NIL) (4 (5 (6) NIL) NIL))
;;
;; What is a node? (elm (left right))
;;
;; This is not consistent - (left right) could not be treated as 'elm'. Let's fix it:
;; (0 (1 (2) (3)) (4 (5 (6))))
;;
;; Great now I have the tree!

;; GENERALIZE

(defun make-bin-leaf (elm)
  "Create a leaf."
  (list elm))

(defun make-bin-node (parent elm1 &optional elm2)
  (list parent elm1 elm2))

(deftest test-make-bin-tree ()
  (check
    (equal (make-bin-node 0 (make-bin-node 1 (make-bin-leaf 2)
                                           (make-bin-leaf 3))
                          (make-bin-node 4 (make-bin-node 5 (make-bin-leaf 6))))
           '(0 (1 (2) (3)) (4 (5 (6) NIL) NIL)) )))

;; INTRODUCE
;;
;; Define what is node and leafs

(defun node-elm (node)
  (first node))

(defun node-left (node)
  (first (second node)))

(defun node-right (node)
  (first (third node)))

(deftest test-nodes ()
  (check
    (eq (node-left '(1 (2) (3))) 2)
    (eq (node-right '(1 (2) (3 (4) (5)))) 3)
    (eq (node-right 1) nil)
    (eq (node-elm '(1)) 1)
    (eq (node-elm nil) nil)))

;;; Predicates

(defun leaf-p (node)
  "Test if binary tree NODE is a leaf."
  (and (listp node)
       (= (list-length node) 1)))

(defun node-p (node)
  "Test if binary tree node is a node with children"
  (not (leaf-p node)))

(defun member-p (elm tree)
  (eq (find-anywhere elm tree) elm))

;; AHA!

;; I discover that it very easy to write 'find-anywhere' when we present trees as nested lists this: (0 (1) (2))
;; 'car' is the current 'cdr's are the successors and we stop when 'car' is an atom. There is no need to remember DFS.

;; (if (atom tree)
;;       (if (eql item tree) tree)
;;       (or (find-anywhere item (first tree))
;;           (find-anywhere item (rest tree))))

(deftest test-member-p ()
  (check
    (eq (member-p 6 *bin-tree*) t)))

;; REMEMBER
;;
;; Do not make big steps test carefully everything!

;; ATTACK
;;
;; Now I have a tree and I can concentrate on solving the problem.
;;
;; SIMPLIFY
;;
;; From where to start? As a rule I start from easiest case - one root and two leafs.
;; In this case task is easy. If (root (p) (q)) then 'root' is the parent.

;; INTRODUCE
;;
;; Names for the searched leafs: 'p' and 'q'
;;
;; Concentrate on (elm (left) (right)) and because this is the simplest case apply rule for the rest.
;; Binary trees are recursive structure so recursion will pass perfectly. Of course be careful how to terminate.

;; Algorithm should be:
;; - find first in a left sub-tree
;; - find second in a right sub-tree
;; - compare to the searched 'p' and 'q'

;; STUCK
;;
;; How to track context? I could find easily element but I have to know its parent.
;; Some how context should be created.
;;
;; Is it possible to use recursion to create context? A tree is a collection of smaller trees
;; up to a simple node. In the same way that a list is a collection of smaller list.

;; We reduce 'elm'(it is tricky to choose proper name) so it is possible to be
;; node, leaf or null. Wait a minute...
;;
;; AHA! I could build the recursive algorithm using possible 'elm' values:
;;                          (lca-rec nil 1 2)
;;                          (lca-rec '(1) 1 2)
;;                          (lca-rec '(0 (1) (2)) 1 2)
;; And I can use them also for tests.

;; Concentrate on the small picture - only one node.

;; if elm is null or leaf
(defun lca-rec (elm p q)
  (cond ((null elm) nil)
        ((or (eq (car elm) p)
             (eq (car elm) q)) elm)
        ;...
        ))

;; More difficult part - if it is a node (elm (p) (q))

(defun lca-rec (elm p q)
  (cond ((null elm) nil)
        ((or (eq (car elm) p)
             (eq (car elm) q)) elm)
        (t (let ((left (cadr elm))      ; ***
                 (right (caddr elm)))   ; ***
             (when (and left right)     ; ***
                 (car elm))))))         ; ***

;; Now I have a tree - the same solution recursively!

(defun lca-rec (elm p q)
  (cond ((null elm) nil)
        ((or (eq (car elm) p)
             (eq (car elm) q)) elm)
        (t (let ((left (lca-rec (cadr elm) p q))     ; ***
                 (right (lca-rec (caddr elm) p q)))  ; ***
             (when (and left right)
               (car elm))))))

;; What we have after recursion? Is it possible to discover nothing?
;; NO! because with recursion we traverse the whole tree. But elements could be on different
;; levels so I somehow to pass trough recursive calls what is found.

;; AHA!
;; Now the better question is when it fails? Only when one or none elements are found.
;; Should I return if only one element is found? NO!

;; REMEMBER. Look at the recursive procedure as simplest part of something big.
;; Recursive call is the glue to the big picture.

;; In our case:
;;
;;... ((left (lca-rec (cadr elm) p q))
;;     (right (lca-rec (caddr elm) p q))) ...
;;
;; finds only 'left' or 'right' of the current node it doesn't search the whole tree. It seems like - but it is not true.
;; That's why I have to return what is fond in current 'node' and the recursion will pass it to the next level until
;; both are found or not. Finally we have:

(defun lca-rec (elm p q)
  (cond ((null elm) nil)
        ((or (eq (car elm) p)
             (eq (car elm) q)) elm)
        (t (let ((left (lca-rec (cadr elm) p q))
                 (right (lca-rec (caddr elm) p q)))
             (if (and left right) (car elm)
                 ;; return what is found until now and hope other will be found on next iteration
                 (or left right))))))        ;; ***

;; What is after recursive call?
