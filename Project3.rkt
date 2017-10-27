#lang racket
;LIST FUNCTIONS
;Append
(define (myappend listA listB)
  (if (empty? listA)
      listB
      (cons (car listA) (myappend (cdr listA) listB))
      )
  )

;Fold-Left
;(Helper)
(define (fold-left-helper start function list value)
  (cond
    [(empty? list) value]
    [(equal? function '-) (fold-left-helper start function (cdr list) (- value (car list)))]
    [(equal? function '+) (fold-left-helper start function (cdr list) (+ value (car list)))]
    [(equal? function '/) (fold-left-helper start function (cdr list) (/ value (car list)))]
    [(equal? function '*) (fold-left-helper start function (cdr list) (* value (car list)))]
    [(equal? function 'quotient) (fold-left-helper start function (cdr list) (quotient value (car list)))]
    [(equal? function 'remainder) (fold-left-helper start function (cdr list) (remainder value (car list)))]
    [(equal? function 'modulo) (fold-left-helper start function (cdr list) (modulo value (car list)))]
    [else (fold-left-helper start function (cdr list) (function value (car list)))]))

;(Main)
(define (fold-left initialValue function list)
  (fold-left-helper initialValue function list initialValue))

;Index Of
;(Helper)
(define (indexOfHelper element list count)
  (cond
    [(empty? list) -1]
    [(eq? element (car list)) count]
    [else (indexOfHelper element (cdr list) (+ count 1))]))

;(Main)
(define (indexOf element list)
  (indexOfHelper element list 0))

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

;REQUIRED FUNCTIONS
;Sum of Factors (helper)
(define (sumOfFactorsHelp num count sum)
  (cond
    [(= num 0) (void)]
    [(= count num) sum]
    [(= (modulo num count) 0) (sumOfFactorsHelp num (+ count 1) (+ sum count))]
    [else (sumOfFactorsHelp num (+ count 1) sum)]))

;Sum of Factors (main)
(define (sumOfFactors num)
  (sumOfFactorsHelp num 1 0))

;Perfect Number
(define (perfect? num)
  (if (= (sumOfFactors num) num)
      #t
      #f))

;Abundant Number
(define (abundant? num)
  (if (> (sumOfFactors num) num)
      #t
      #f))

;Deficient Number
(define (deficient? num)
  (if (< (sumOfFactors num) num)
      #t
      #f))

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

;Reverse
(define (myreverse listR)
  (if (= (mycardinality listR) 1)
      listR
      (cons (myreverse (cdr listR)) (car listR))))

;Runs the List Functions (Append, IndexOf, Fold-Left)
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

;Runs IndexOf
(display "Index of an element")
(display "\n")
(display "Enter element: ")
(define indexElement (read))
(display "Enter List: ")
(define indexList (read))
(indexOf indexElement indexList)
(display "\n")

;Runs Fold-Left
(display "Runs Fold-Left")
(display "\n")
(display "Enter Initial Value: ")
(define initialValue (read))
(display "Enter Function: ")
(define functionFold (read))
(display "Enter List: ")
(define foldList (read))
(fold-left initialValue functionFold foldList)
(display foldList)
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

;Runs the Required Mathematical Functions
;Perfect Number
(display "Enter a number to determine whether it is a perfect number: ")
(define perfectTest (read))
(display "Value: ")
(perfect? perfectTest)
(display "\n")

;Abundant Number
(display "Enter a number to determine whether it is a abundant number: ")
(define abundantTest (read))
(display "Value: ")
(abundant? abundantTest)
(display "\n")

;Deficient Number
(display "Enter a number to determine whether it is a deficient number: ")
(define deficientTest (read))
(display "Value: ")
(deficient? deficientTest)
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
(myright-tri tri-num1 tri-num2 tri-num3)
(display "\n")

;Run Reverse
(display "Reverse a list")
(display "\n")
(display "Create a list: ")
(define listR (read))
(display "The reverse of that list is: ")
(myreverse listR)
(display "\n")
