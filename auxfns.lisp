;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AUX; Base: 10 -*-

;;;; File auxfns.lisp: Auxiliary functions - some are taken from PAIP

(in-package #:aux)

(defun declare-ignore (&rest args)
  "Ignore the arguments."
  (declare (ignore args))
  nil)

(defun true (&rest args) "Always return true." (declare (ignore args)) t)

(defun false (&rest args) "Always return false." (declare (ignore args)) nil)

(defun nothing (&rest args)
  "Don't do anything, and return nil."
  (declare (ignore args))
  nil)

(defun not-null (x) (not (null x)))

(defconstant fail nil "Indicates failure")

(defun is (value)
  #'(lambda (x) (eql x value)))

;;; ____________________________________________________________________________
;;;                                               The Debugging Output Facility

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(defun debugit (&rest ids)
  "Start dbg output on the given IDS."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebugit (&rest ids)
  "Stop dbg on the `ids'. With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

;;; ____________________________________________________________________________

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of SEQUENCE that match ITEM,
according to the keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun mappend (fn list)
  "Append the results of calling FN on each element of LIST.
Like `mapcon', but uses `append' instead of `nconc'."
  (apply #'append (mapcar fn list)))

(defun mklist (x)
  "If X is a list return it, otherwise return the list of X"
  (if (listp x) x (list x)))

(defun flatten (exp)
  "Get rid of imbedded lists (to one level only)."
  (mappend #'mklist exp))

(defun random-elt (seq)
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))

(defun compose (&rest functions)
  "Return the function that is the composition of all the args.
i.e. (compose f g h) = (lambda (x) (f (g (h x))))."
  (case (length functions)
    (0 #'identity)
    (1 (first functions))
    (2 (let ((f (first functions))
             (g (second functions)))
         #'(lambda (x) (funcall f (funcall g x)))))
    (t #'(lambda (x)
           (reduce #'funcall functions :from-end t
                   :initial-value x)))))

(defun range (max &key (min 0) (step 1))
  "Generates list that contains range of numbers with MAX included."
  (loop
     for n from min below max by step
     collect n))

;;; ____________________________________________________________________________
;;;                                                    The Memoization Facility

(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of FN."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace FN-NAME's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

;;; ____________________________________________________________________________
;;;                                                         Delayed Computation

(defstruct delay value (computed? nil))

(defmacro delay (&rest body)
  "A computation that can be executed later by `force'."
  `(make-delay :value #'(lambda () . ,body)))

(defun force (delay)
  "Do a delayed computation, or fetch its previously-computed value."
  (if (delay-computed? delay)
      (delay-value delay)
      (prog1 (setf (delay-value delay) (funcall (delay-value delay)))
        (setf (delay-computed? delay) t))))

;;; ____________________________________________________________________________
;;;                                                                       Trees

(defun gen-successors (x)
  (list (* 2 x) (+ 1 (* 2 x))))

(defun finite-binary-tree (n)
  "Return a successor function that generates a binary tree
with N nodes."
  #'(lambda (x)
      (remove-if #'(lambda (child) (> child n))
                 (gen-successors x))))

(defun tree-search (states goal-p successors-fn combiner-fn)
  "Find a state that satisfies GOAL-P. Start with STATES,
and search according to SUCCESSORS-FN and COMBINER-FN."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
            (funcall combiner-fn
                     (funcall successors-fn (first states))
                     (rest states))
            goal-p successors-fn combiner-fn))))

(defun find-anywhere (item tree)
  "Does ITEM occur anywhere in TREE?"
  (if (atom tree)
      (if (eql item tree) tree)
      (or (find-anywhere item (first tree))
          (find-anywhere item (rest tree)))))

(defun find-if-anywhere (predicate tree)
  "Does PREDICATE apply to any atom in the TREE?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))


;;; ____________________________________________________________________________
;;;                                                                      Queues

;;; A queue is a (last . contents) pair

(defun queue-contents (q) (cdr q))

(defun make-queue ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  "Insert ITEM at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun dequeue (q)
  "Remove an item from the front of the queue."
  (pop (cdr q))
  (if (null (cdr q)) (setf (car q) q))
  q)

(defun front (q) (first (queue-contents q)))

(defun empty-queue-p (q) (null (queue-contents q)))

(defun queue-nconc (q list)
  "Add the elements of LIST to the end of the queue."
  (setf (car q)
        (last (setf (rest (car q)) list))))
