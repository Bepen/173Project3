#lang racket
;List Functions
;Append Two Lists

;Set Functions
;Cardinality
(define (cardinalityHelp setHelp counter)
  (if (empty? setHelp)
      counter
      (cardinalityHelp (set-rest setHelp) (+ counter 1))))

(define (mycardinality set)
  (cardinalityHelp set 0))

;MATH FUNCTIONS
;Absolute Value Method
(define (abs inp)
  (if (< inp 0)
      (* inp -1)
      inp))
;Factorial Method (prints nothing if less than 0)
(define (factorial inp)
  (if (< inp 0)
      (void)
      (if (= inp 0)
          1
          (* inp (factorial (- inp 1))))))
;GCD Method (Euclidean Algorithm)
(define (gcd inp1 inp2)
  (cond
    [(= inp1 0) inp2]
    [(= inp2 0) inp1]
    [else (gcd inp2 (modulo inp1 inp2))]))

;LCM Method
(define (lcm inp1 inp2)
  (/ (* inp1 inp2) (gcd inp1 inp2)))


;Runs the Set Functions (TBD)
;Run Cardinality
(display "Enter a set to find the cardinality of: ")
(define readSet (read))
(display "Cardinality: ")
(mycardinality readSet)
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
