#lang racket
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
(define readFactNum1 (read))
(display "Second Number: ")
(define readFactNum2 (read))
(display "GCD: ")
(gcd readFactNum1 readFactNum2)
(display "\n")