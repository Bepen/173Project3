#lang racket
;LIST FUNCTIONS
;Append
(define (myappend listA listB)
  (if (empty? listA)
      listB
      (cons (car listA) (myappend (cdr listA) listB))
      )
  )

;SET FUNCTIONS
;Subset
(define (mysubset? sub super)
  (cond
    [(= (mycardinality sub) 0) #t]
    [(not (mymember (set-first sub) super)) #f]
    [else (subset? (set-rest sub) super)]))

;Membership
(define (mymember element set)
  (cond
    [(= (mycardinality set) 0) #f]
    [(eq? element (set-first set)) #t]
    [else (mymember element (set-rest set))]))

;Cardinality
;(Helper)
(define (cardinalityHelp setHelp counter)
  (if (empty? setHelp)
      counter
      (cardinalityHelp (set-rest setHelp) (+ counter 1))))
;(Main)
(define (mycardinality set)
  (cardinalityHelp set 0))

;MATH FUNCTIONS
;Absolute Value
(define (abs inp)
  (if (< inp 0)
      (* inp -1)
      inp))
;Factorial (prints nothing if less than 0)
(define (factorial inp)
  (if (< inp 0)
      (void)
      (if (= inp 0)
          1
          (* inp (factorial (- inp 1))))))
;GCD (Euclidean Algorithm)
(define (gcd inp1 inp2)
  (cond
    [(= inp1 0) inp2]
    [(= inp2 0) inp1]
    [else (gcd inp2 (modulo inp1 inp2))]))

;EXTRA CREDIT
;LCM
(define (lcm inp1 inp2)
  (/ (* inp1 inp2) (gcd inp1 inp2)))

;Superset
(define (mysuperset? super sub)
  (mysubset? sub super))

;Pythogorean Triple
(define (myright-tri num1 num2 num3)
  (cond
    [(<= num1 0) #f]
    [(<= num2 0) #f]
    [(<= num3 0) #f]
    [(= (+ (* num1 num1) (* num2 num2)) (* num3 num3)) #t]
    [else #f]))

;Runs the List Functions (TBD)
;Runs Append
(display "Append two lists")
(display "\n")
(display "Create the first list: ")
(define listA (read))
(display "Create the second list: ")
(define listB (read))
(display "Appending the first and second list: ")
(myappend listA listB)
(display "\n")

;Runs the Set Functions (Membership, Cardinality, Subtset)
;Runs Membership
(display "Testing membership of element in a set");
(display "\n")
(display "Enter element to test membership of: ")
(define element (read))
(display "Enter set: ")
(define set (read))
(display "Printing membership status: ")
(mymember element set)
(display "\n")

;Run Cardinality
(display "Enter a set to find the cardinality of: ")
(define readSet (read))
(display "Cardinality: ")
(mycardinality readSet)
(display "\n")

;Runs Subset
(display "Determining if one set is a subset of another")
(display "\n")
(display "Create the first set (subset): ")
(define sub (read))
(display "Create the second set (superset): ")
(define super (read))
(display "Is the first set a subset of the second? : ")
(mysubset? sub super)
(display "\n")

;Runs the Math Functions (Absolute Value, Factorial, GCD)
;Run Absolute Value
(display "Enter a number to find its absolute value: ")
(define readAbsNum (read))
(display "Absolute Value: ")
(abs readAbsNum)
(display "\n")

;Run Factorial
(display "Enter a number to find its factorial value: ")
(define readFactNum (read))
(display "Factorial: ")
(factorial readFactNum)
(display "\n")

;Run GCD
(display "Finding GCD between 2 numbers:")
(display "\n")
(display "First Number: ")
(define readGCDNum1 (read))
(display "Second Number: ")
(define readGCDNum2 (read))
(display "GCD: ")
(gcd readGCDNum1 readGCDNum2)
(display "\n")

;Extra Credit
(display "Extra Credit Questions")
(display "\n")
;Run LCM (Extra Credit)
(display "Finding LCM between 2 numbers:")
(display "\n")
(display "First Number: ")
(define readLCMNum1 (read))
(display "Second Number: ")
(define readLCMNum2 (read))
(display "LCM: ")
(lcm readLCMNum1 readLCMNum2)
(display "\n")

;Runs Subset (Extra Credit)
(display "Determining if one set is a superset of another")
(display "\n")
(display "Create the first set (superset): ")
(define superEx (read))
(display "Create the second set (subset): ")
(define subEx (read))
(display "Is the first set a superset of the second? : ")
(mysuperset? superEx subEx)
(display "\n")

;Runs Pythagorean Triple (Extra Credit)
(display "Determining if three numbers are part of a pythagorean triple")
(display "\n")
(display "Enter first number: ")
(define tri-num1 (read))
(display "Enter second number: ")
(define tri-num2 (read))
(display "Enter third number: ")
(define tri-num3 (read))
(display "Are these three numbers part of a pythagorean triple: ")
(myright-tri tri-num1 tri-num1 tri-num1)
(display "\n")
