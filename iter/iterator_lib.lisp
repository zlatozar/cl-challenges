;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ITER-LIB; Base: 10 -*-

(in-package :iter-lib)

;; Convert following code to use iterate library

;;; ____________________________________________________________________________

(loop for i
   below 10
   sum i)

;; Loop with 'from' to 'to'
(loop for i
   from 10
   to 20
   sum i)

;; Loop using 'in' to iterate through a list
(loop for i
   in '(10 20 30 40 50 60 70 80 90 100)
   sum i)

;; Example of do'ing' something in a loop
(loop for i
   below 30
   do (princ i))

;; Only do stuff 'when' it meets a certain criteria
;; In this case: 'when' it is even
(loop for i
   below 31
   when (evenp i)
   sum i)

;; Break out of loop with 'return' 'when'
(loop for i
   from 10
   to 2000
   do (print i)
   when (= i 15)
   return 'CrazyCodeStopNow!)

;; 'Collect' values into a list
(loop for i
   in '(1 3 5 7 9)
   collect (* i i))

;; Mulitple 'for's in one loop and incrementing there values
;; at the same time. [They do not loop independently, lowest
;; value exits the loop]
(loop for x below 12
   for y below 12
   collect x
   collect y)

;; Same as above except mulitpling the collected values
(loop for x below 12
   for y below 12
   collect (* x y))

;; Nested loop going through multiplication table products 1-12
(loop for x from 1 to 12
   collect (loop for y from 1 to 12
              collect (* x y)))

;; Collect months and their number value in a dotted-list
(loop for i
   from 1
   for month
   in '(jan feb mar apr may jun jul aug sep oct nov dec)
   collect (cons i month))

;; Repeat loop
(loop repeat 8
   do (print "Could you repeat that?"))

;; Give an initial duty before looping through the rest
(loop initially
     (print "Wait! I don't even know your name yet...")
   for x below 6
   do (print "Say my name %!+(&"))

;; Give a final expression after you do the dues
(loop for x below 4
   do (print "Oh... Don't stop!")
   finally
     (print "What! You are finished!"))

;; Initially and finally in same loop
(loop initially
     (print "If I had a nickel for...")
   for x below 7
   do (print "Cha ching")
   finally
     (print "I'm rich!"))

;; Loops can be 'named' and then broken out of with 'return-from'.
;; In this case the second-loop never gets to print out it's third line.
;; Although it is broken out of, it never prints 'exited-second-loop
;; for some reason. Maybe cause the 'first-loop' keeps going and
;; keeps calling the second-loop again.
(loop named first-loop
   for i from 1 to 3
   do (print i)
   do (princ "first loop")
     (loop named second-loop
        for j from 1 to 3
        do (print j)
        do (princ "second loop")
        when (= j 2)
        do (return-from second-loop 'exited-second-loop)))

;; Exits from the first loop returning EXITED-FIRST-LOOP
(loop named first-loop
   for i from 1 to 3
   do (print i)
   do (princ "first loop")
     (loop named second-loop
        for j from 1 to 3
        do (print j)
        do (princ "second loop")
        when (= i 2)
        do (return-from first-loop 'exited-first-loop)))

;; This loop will run as long as it comes across an odd number
(loop for i in '(1 3 5 7 9 10 12 13 15 17)
   while (oddp i)
   do (print "Yep! I'm still an odd ball: ")
   do (princ i))

;; 'Until' this loop has i greater than 9, keep going
(loop for i
   from 0
   do (print "I'm not high enough yet: ")
   do (princ i)
   until (> i 9))

;; Example of using 'with' to set an auxiliary variable in a loop
(loop with x = (* 2 2)
   repeat 6
   do (print x))

;; Loop through hash table with 'being' , 'each', 'using'
;; When using 'each' 'hash-key' is singular

(defparameter cookie-jar (make-hash-table))

(setf (gethash 'oreo cookie-jar) "yucky")
(setf (gethash 'chocolate-chip cookie-jar) "yummy")
(setf (gethash 'oatmeal cookie-jar) "ugh")

(loop for person being each hash-key of cookie-jar
   using (hash-value tasted)
   do (print (cons person tasted)))

;; Loop through hash table with 'being' 'the'
;; When using 'the' 'hash-keys' is plural
(defparameter tv-shows (make-hash-table))
(setf (gethash 'Breaking-Bad tv-shows) 10)
(setf (gethash 'Shameless tv-shows) 10)
(setf (gethash 'Arrow tv-shows) 5)
(setf (gethash 'Hell-on-Wheels tv-shows) 7)
(setf (gethash 'The-Walking-Dead tv-shows) 7)

(loop for series being the hash-keys of tv-shows
   do (print series))

(loop for rating being the hash-values of tv-shows
   do (print rating))

(loop for i from 0
   do (print i)
   when (= i 5)
   return 'yippy!)

;; Collects x into a list
(loop as x from 2 to 10
   collect x)

;; Sums the numbers in the list
(loop for i
   in '(200 125 75)
   sum i)

(loop for x
   on '(10 20 30)
   do (print x))
;; Count by 2 and sum them
(loop for i from 2 to 18
   by 2
   do (print i)
   sum i)

(loop repeat 6
   for x = 18.0
   then (/ x 2)
   collect x)

(loop for i
   upfrom 4 to 12
   do (print i)
   sum i)

(loop for i from 4
   upto 12
   sum i)

(loop for i
   downfrom 10 to 0
   do (print i))

(loop for i from 10
   downto 3
   do (print i))

;; Loop 'across' an array
(loop for i
   across #(20 40 60)
   sum i)

;; 'Into' lets variable be created, that can be returned
(loop for i
   in '(4 12 77 1 -23)
   minimize i into lowest
   maximize i into biggest
   finally (return (cons lowest biggest)))

;; Print out even numbers using 'unless'
(loop for i below 12
   unless (oddp i)
   do (print i))

;; Print out odd numbers below 15 with 'if'
(loop for i below 15
   if (oddp i) do (print i))

;; 'When' odd print number, when even print me not odd
(loop for i below 15
   when (oddp i) do (print i)
   when (evenp i) do (print "me not odd"))

;; Sum the numbers below 12
(loop for i below 12
   sum i)

;; Set i to the lowest number in list
(loop for i
   in '(5 6 7 2 3 4 8 9)
   minimize i)

;; Set i to max number in list
(loop for i
   in '(99 77 23 32 0 55)
   maximize i)

;; Check if all items in list are 'always' even
(loop for i
   in '(4 6 8 10 12 14)
   always (evenp i))

;; Make sure items in list are 'never' odd
(loop for i
   in '(4 6 8 10 12 14)
   never (oddp i))

;; Check to see if 'thereis' and odd number
(loop for i
   in '(53 2324 4737 23723 324)
   thereis (oddp i))

;; Loop with 'and'
(loop for x below 6
   when (= x 4) do (print "i am four")
   and do (print "I like fours!")
   do (print "I wish I was always a four"))

;; Loop with an 'else'
(loop for i below 10
   if (oddp i) do (print i)
   else do (print "I'm even yo..."))

;; Use 'end' to denote to stop when clause
(loop for i below 8
   when (oddp i) do (print i)
   end
   do (print "yo you!"))

;; 'Append' to list inside loop
(loop for i below 12
   append (list 'y i))

;; 'Nconc' a list inside a loop
(loop for i below 12
   nconc (list 'y i))

;;; TEST




;;; ____________________________________________________________________________
;;;                                                                    My notes
