#lang racket

;The following two lines are required to test the code.
(require rackunit)
(require rackunit/text-ui)

#|
CS 270
Homework 5
Create By Professor Bruce Char, Professor Mark Boady, and Professor Jeremy Johnson

Complete each of the below functions.

Tests given are not resigned to be comprehensive.
They will give you an idea if your code is write, but they do not test all possible cases.

Think about your design.

When grading, we may add additional tests for your functions.

Once you write a function, you may use it in later questions.

Important Rules:
1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
    Recursive helper functions are allowed (the main function not being recursive).
3.) You may not use the set! command. If used, your answer will get a zero.
4.) Using If/Cond to explicitly pass tests instead of following the instructions
    will always result in a zero for that question.
5.) Use #t and #f instead of true or false. Racket treats them differently
   in some special cases.
|#

;-----------------------------------------------------------
;----------------- PART 1 ----------------------------------
;-----------------------------------------------------------

;Question 1
; We can think of a polynomial as a list.
; For example, you have the polynomial
; 5 + 2x^2 - 3x^3
; The largest exponent here is x^3.
; We can describe this as just 4 coefficents
; (5 0 2 -3)
; The first coefficient 5 goes with 5*x^0=5
; The second coefficient 0 goes with 0*x^1 = 0
; The third coefficient 2 goes with 2*x^2 = 2x^2
; The fourth coefficient -3 goes with -3*x^3 = -3x^3
; If we add them together we get 5 + 2x^2 - 3x^3

; The coefficient list will never have empty spaces.
; The first position is always the coefficient of x^0 then x^1 etc.

; Given a polynomial as a list of coefficients
; evaluate it at a point.
; (evaluate '(5 0 2 -3) 2)
; will compute:
; 5 + 0*2 + 2*2^2 + -3*(2^3) = -11

;Create a recursive function to evaluate polynomials
;input-contract:
;          First input is a polynomial described as a list of coefficients
;          Second input is value of x to evaluate at
;output-contract: An integer, the result of evaluating the polynomial at point x
;Note: when done efficiently, there is no need for the expt function
;This function **MUST** be implemented recursively (or using a recursive helper function).
(define (evaluate poly point)
  (define (helper poly accumulator)
    (if (empty? poly)
        accumulator
        (helper (rest poly)
                (+ (* accumulator point) (first poly)))))
  
  (helper (reverse poly) 0))

;Tests
(define-test-suite testeval
  (check-equal?
    (evaluate '(5 0 2 -3) 2)
    -11)
  (check-equal?
    (evaluate '(5 0 2 -3) 10)
    -2795)
  (check-equal?
   (evaluate '(0 0 0 0 7) 2)
   112)
  (check-equal?
   (evaluate '(9 8 7 6 5 4 3 2 1) 9)
   54481005)
  (check-equal?
   (evaluate '(0 1 0 2 0 3 0 4) 20)
   5129616020
  )
  (check-equal?
   (evaluate '(-1 1 -1 1) -12)
   -1885
  )
  (check-equal?
   (evaluate '(2 4 6 8 10) 7)
   27078
  ) 
  (check-equal?
   (evaluate '(1 3 5 7 9) 1)
   25
  ) 
  (check-equal?
   (evaluate '(1 2 3 4 5 6 7 8 9 10) 1)
   55
  ) 
  (check-equal?
   (evaluate '(7 7 1 0 0 0 9) 1)
   24
  )  
)
(display "Question 1 evaluate (10 points)")
(newline)
(define q1_score (- 10 (run-tests testeval 'verbose)))


;Question 2
;Given two polynomials, we can add them by adding the coefficients
;For example,
; Let A = 5 + x^1 + 3x^3 (represented as (5 1 0 3)
; Let B = 9 + x^1 + 6x^2 + 5x^4 (represented as (9 1 6 0 5)
; When we add A+B we get:
; (5+9) + (1+1)x^1 + (6+0)x^2 + (3+0)x^3 + 5x^4
; 14 + 2x^1 + 6x^2 + 3x^3 + 5x^4
; represented as (14 2 6 3 5)

;Write a function to add to polynomials
;input-contract: A and B are polynomials represented as
;                lists of coefficients
;output-contract: A+B as a list of coefficents
;This function **MUST** be implemented recursively.
(define (addPoly A B)
  (cond
    [(empty? A) B]
    [(empty? B) A]
    [else (cons (+ (first A) (first B))
                (addPoly (rest A) (rest B)))]))

;Tests
(define-test-suite testaddpoly
  (check-equal?
    (addPoly '(5 1 0 3) '(9 1 6 0 5))
    '(14 2 6 3 5)
  )
  (check-equal?
   (addPoly '(0 0 5) '(7))
   '(7 0 5)
  )
  (check-equal?
   (addPoly '(12) '(8 9 10))
   '(20 9 10)
  )
  (check-equal?
   (addPoly '(-1 1 -1 1) '(1 -1 1 -1))
   '(0 0 0 0)
  )
  (check-equal?
   (addPoly '(9 12 15 7) '(11 8 5 13))
   '(20 20 20 20)
  )
  (check-equal?
   (addPoly '(1 2 3 4 5 6 7 8 9 10 11 12) '(12 11 10 9 8 7 6 5 4 3 2 1))
   '(13 13 13 13 13 13 13 13 13 13 13 13)
  )
  (check-equal?
   (addPoly '(0 0 0 1) '(1 0 0 0))
   '(1 0 0 1)
  )
  (check-equal?
   (addPoly '() '(1 2 3 4))
   '(1 2 3 4)
  )
  (check-equal?
   (addPoly '(1 2 3 4) '())
   '(1 2 3 4)
  )
  (check-equal?
   (addPoly '(7) '(3))
   '(10)
  )
)
(display "Question 2 addPloy (10 points)")
(newline)
(define q2_score (- 10 (run-tests testaddpoly 'verbose)))


;Question 3
; We can multiply a polynomial by a constant.
; For example:
; We have a polynomial
; 7x+9x^3 represented as (0 7 0 9)
; We want to multiply the entire polynomial by 2
; 2*(7x+9x^3) = 14x+18x^3
; represented as (0 14 0 18)

;Implement a function to multiply a polynomial by a constant
;input-contract: an integer and a polynomial represented as
;    a list of coefficients
;output-contract: a polynomial represented as a list of coefficients
;requirement: to get practice using some of the higher order functions we don't
;ordinarily use (map/foldr/foldl/lambda),implement this function WITHOUT recursion or any helpers.

(define (multCoeff val poly)
  (map (lambda (x) (* val x)) poly))

;Tests
(define-test-suite testmultcoeff
  (check-equal?
    (multCoeff 2 '(0 7 0 9))
    '(0 14 0 18)
  )
  (check-equal?
   (multCoeff 10 '(1 2 3 4 5 6 7 8 9 10))
   '(10 20 30 40 50 60 70 80 90 100)
  )
  (check-equal?
   (multCoeff -1 '(12 5 9 -2 -3 -5))
   '(-12 -5 -9 2 3 5)
  )
  (check-equal?
   (multCoeff (/ 3 2) '(4 6 9 5 11))
   '(6 9 27/2 15/2 33/2)
  )
  (check-equal?
   (multCoeff 0 '(1 2 3 0 0 9))
   '(0 0 0 0 0 0)
  )
)
(display "Question 3 multCoeff (5 points)")
(newline)
(define q3_score (- 5 (run-tests testmultcoeff 'verbose)))


;Question 4
; We can also multiply a polynomial by x
; This increases all the exponents by 1
; For Example,
; We have the polynomial x+2x^2+3^3
; represented as '(0 1 2 3)
; We want to compute
; x*(x+2x^2+3x^3)
; = x^2+2x^3+3x^3
; Represented as the list
; '(0 0 1 2 3)

;Write a function that multiplies the polynomial by x
;input-contract: a polynomial as a list of coefficients
;output-contract: a polynomial as a list of coefficients
(define (multByX poly)
  (cons 0 poly))

;Tests
(define-test-suite testmultbyx
  (check-equal?
    (multByX '(0 1 2 3))
    '(0 0 1 2 3)
  )
  (check-equal?
   (multByX '(1 1 1 1))
   '(0 1 1 1 1)
   )
  (check-equal?
   (multByX '(2))
   '(0 2)
  )
  (check-equal?
   (multByX '(100 200 599 -7 -12 -100))
   '(0 100 200 599 -7 -12 -100)
  )
  (check-equal?
   (multByX '(10 9 8 7 6))
   '(0 10 9 8 7 6)
  )
)
(display "Question 4 multByX (5 points)")
(newline)
(define q4_score (- 5 (run-tests testmultbyx 'verbose)))


;Question 5
; We can multiply two polynomials
; A= 2 + x - 2x^2 (2 1 -2)
; B= 9x + 3x^2 (0 9 3)
; We want to compute A*B = C
; C = 18x + 15x^2 -15x^3 -6x^4
; as a list this is (0 18 15 -15 -6)


;Write a function to multiply two polynomials
;input-contract: P and Q are racket representations of the polynomials p(x) and q(x)
;output-contract: (multPoly P Q) is the racket representation of the polynomial p(x)*q(x)
;HINT: functions you wrote earlier in this HW will be helpful
;This function **MUST** be implemented recursively.
(define (multPoly P Q)
  (if (empty? P)
      '()
      (addPoly (multCoeff (first P) Q)
               (multByX (multPoly (rest P) Q)))))

;Tests
(define-test-suite testmultpoly
  (check-equal?
    (multPoly '(2 1 -2) '(0 9 3))
    '(0 18 15 -15 -6)
    )
  (check-equal?
   (multPoly '(7) '(2))
   '(14)
   )
  (check-equal?
   (multPoly '(1 1) '(2 2))
   '(2 4 2)
  )
  (check-equal?
   (multPoly '(-1 1) '(1 -1))
   '(-1 2 -1)
  )
  (check-equal?
   (multPoly '(-1 1) '(-1 -1))
   '(1 0 -1)
  )
  (check-equal?
   (multPoly '(5 9 7 3 1) '(0 2))
   '(0 10 18 14 6 2)
  )
  (check-equal?
   (multPoly '(0 0 1) '(9 11 14 -3 7 15))
   '(0 0 9 11 14 -3 7 15)
  )
  (check-equal?
   (multPoly '(2) '(3 5 7 9))
   '(6 10 14 18)
  )
  (check-equal?
   (multPoly '(3 5 7 9) '(2))
   '(6 10 14 18)
  )
  (check-equal?
   (multPoly '(1 1 1 1 1 1 1) '(1 -1 1 -1 1 -1 1 -1 1 -1))
   '(1 0 1 0 1 0 1 -1 1 -1 0 -1 0 -1 0 -1)
  )
)

(display "Question 5 multPoly (10 points)")
(newline)
(define q5_score (- 10 (run-tests testmultpoly 'verbose)))


;-----------------------------------------------------------
;----------------- PART 2 ----------------------------------
;-----------------------------------------------------------

;Question 6
; Write a recursive function all_q to check if a list of symbols
; contains all q symbols.
; Don't forget the base case and the necessary recursion.
; You may use any previously written function.

;Hint: You can use equal? on symbols
;(equal? 'q 'b) returns #f
;(equal? 'b 'b) returns #t

; Check if a list contains all q characters
; Input:  L is any list.
; Output: a boolean value which is true when there
;          does not exist any non-q elements in the list.
;This function **MUST** be implemented recursively.
(define (all_q L)
  (cond
    [(empty? L) #t]
    [(equal? (first L) 'q) (all_q (rest L))]
    [else #f]))

(define-test-suite test_all_q
  (check-equal? (all_q '(q x)) #f)
  (check-equal? (all_q '(q)) #t)
  (check-equal? (all_q '(b)) #f)
  (check-equal? (all_q '(b c)) #f)
  (check-equal? (all_q '(c q)) #f)
  (check-equal? (all_q '(q q)) #t)
  (check-equal? (all_q '(q x q)) #f)
  (check-equal? (all_q '(q q q)) #t)
  (check-equal? (all_q '(q q q q q q q q)) #t)
  (check-equal? (all_q '(q q q q w q q q)) #f)
)

(display "Question 6 all_q (10 points)")
(newline)
(define q6_score (- 10 (run-tests test_all_q 'verbose)))

;Question 7
;Solve question 6 again. This time you may not write a recursive function.
;Solve the problem using map/foldr/foldl/lambda instead of recursion.
;Do not write helper functions for this question. Use Lambda instead.

; Check if a list contains all q characters
; Input:  L is a list.
; Output: a boolean value which is true when there
;          does not exist any non-q elements in the list.
(define (all_q_v2 L)
  (foldl (lambda (x y) (and x y)) #t (map (lambda (x) (equal? x 'q)) L)))


(define-test-suite test_all_q_v2
  (check-equal? (all_q_v2 '()) #t)
  (check-equal? (all_q_v2 '(q)) #t)
  (check-equal? (all_q_v2 '(b)) #f)
  (check-equal? (all_q_v2 '(b c)) #f)
  (check-equal? (all_q_v2 '(c q)) #f)
  (check-equal? (all_q_v2 '(q q)) #t)
  (check-equal? (all_q_v2 '(q x q)) #f)
  (check-equal? (all_q_v2 '(q q q)) #t)
  (check-equal? (all_q_v2 '(q q q q q q q q)) #t)
  (check-equal? (all_q_v2 '(q q q q w q q q)) #f)
)

(display "Question 7 all_q_v2 (10 points)")
(newline)
(define q7_score (- 10 (run-tests test_all_q_v2 'verbose)))


;Question 8
; Write a recursive function at_least_one_q to check if a list of symbols
; contains at least one q symbol.
; Don't forget the base case and the necessary recursion. 
; You may use any previously written function.

; Check if a list contains at least one q
; Input:  L is a list.
; Output: a boolean value which is true when at least one of the elements
;          in L is equal to q and false otherwise.
;This function **MUST** be implemented recursively.
(define (at_least_one_q L)
  (cond
    [(empty? L) #f]
    [(equal? (first L) 'q) #t]
    [else (at_least_one_q (rest L))]))

(define-test-suite test_at_least_one_q
  (check-equal? (at_least_one_q '(q)) #t)
  (check-equal? (at_least_one_q '(b)) #f)
  (check-equal? (at_least_one_q '(x y)) #f)
  (check-equal? (at_least_one_q '(v q)) #t)
  (check-equal? (at_least_one_q '(q q)) #t)
  (check-equal? (at_least_one_q '(x x d)) #f)
  (check-equal? (at_least_one_q '(c t q)) #t)
  (check-equal? (at_least_one_q '(q q q)) #t)
  (check-equal? (at_least_one_q '(q w q q)) #t)
  (check-equal? (at_least_one_q '(x y d w)) #f)
)

(display "Question 8 at_least_one_q (10 points)")
(newline)
(define q8_score (- 10 (run-tests test_at_least_one_q 'verbose)))


;Question 9
;Solve question 8 again. This time you may not write a recursive function.
;Solve the problem using map/foldr/foldl/lambda instead of recursion.
;Do not write helper functions for this question. Use Lambda instead.

; Check if a list contains at least one q
; Input:  L is a list.
; Output: a boolean value which is true when at least one of the elements
;          in L is equal to q and false otherwise.
(define (at_least_one_q_v2 L)
  (foldl (lambda (x acc)
           (or (equal? x 'q) acc))
         #f
         L))



(define-test-suite test_at_least_one_q_v2
  (check-equal? (at_least_one_q_v2 '(q)) #t)
  (check-equal? (at_least_one_q_v2 '(b)) #f)
  (check-equal? (at_least_one_q_v2 '(x y)) #f)
  (check-equal? (at_least_one_q_v2 '(v q)) #t)
  (check-equal? (at_least_one_q_v2 '(q q)) #t)
  (check-equal? (at_least_one_q_v2 '(x x d)) #f)
  (check-equal? (at_least_one_q_v2 '(c t q)) #t)
  (check-equal? (at_least_one_q_v2 '(q q q)) #t)
  (check-equal? (at_least_one_q_v2 '(q w q q)) #t)
  (check-equal? (at_least_one_q_v2 '(x y d w)) #f)
)

(display "Question 9 at_least_one_q_v2 (10 points)")
(newline)
(define q9_score (- 10 (run-tests test_at_least_one_q_v2 'verbose)))


;Question 10
; Write a recursive function exactly_one_q to check if a list of symbols
; contains exactly one q symbol.
; Don't forget the base case and the necessary recursion.
; You may use any previously written function.

;Hint: The answer to question 8 is helpful to use here. Done properly, there is no need to use
; arithmetic here. (i.e. creating an accumulator that counts the numbers of q's is extremely inefficient)

; Check if a list contains exactly one q
; Input:  L is a list.
; Output: a boolean value which is true when exactly one of the elements
;          in L is equal to q and false otherwise.
;This function **MUST** be implemented recursively.
(define (exactly_one_q L)
  (define (count-q L)
    (if (empty? L)
        0
        (+ (if (equal? (first L) 'q) 1 0)
           (count-q (rest L)))))
  (= 1 (count-q L)))

(define-test-suite test_exactly_one_q
  (check-equal? (exactly_one_q '(q)) #t)
  (check-equal? (exactly_one_q '(x)) #f)
  (check-equal? (exactly_one_q '(z r)) #f)
  (check-equal? (exactly_one_q '(q d)) #t)
  (check-equal? (exactly_one_q '(q q)) #f)
  (check-equal? (exactly_one_q '(d e p)) #f)
  (check-equal? (exactly_one_q '(q b q)) #f)
  (check-equal? (exactly_one_q '(q q q)) #f)
  (check-equal? (exactly_one_q '(q n q q)) #f)
  (check-equal? (exactly_one_q '(m n m q)) #t)
)

(display "Question 10 exactly_one_q (10 points)")
(newline)
(define q10_score (- 10 (run-tests test_exactly_one_q 'verbose)))

;Question 11
;Solve question 10 again. This time you may not write a recursive function.
;Solve the problem using map/foldr/foldl/lambda instead of recursion.
;Do not write helper functions for this question. Use Lambda instead.

; Check if a list contains exactly one q
; Input:  L is a list.
; Output: a boolean value which is true when exactly one of the elements
;          in L is equal to q and false otherwise.
(define (exactly_one_q_v2 L)
  (= 1 (foldr + 0 (map (lambda (x) (if (equal? x 'q) 1 0)) L))))

(define-test-suite test_exactly_one_q_v2
  (check-equal? (exactly_one_q_v2 '(q)) #t)
  (check-equal? (exactly_one_q_v2 '(x)) #f)
  (check-equal? (exactly_one_q_v2 '(z r)) #f)
  (check-equal? (exactly_one_q_v2 '(q d)) #t)
  (check-equal? (exactly_one_q_v2 '(q q)) #f)
  (check-equal? (exactly_one_q_v2 '(d e p)) #f)
  (check-equal? (exactly_one_q_v2 '(q b q)) #f)
  (check-equal? (exactly_one_q_v2 '(q q q)) #f)
  (check-equal? (exactly_one_q_v2 '(q n q q)) #f)
  (check-equal? (exactly_one_q_v2 '(m n m q)) #t)
)

(display "Question 11 exactly_one_q_v2 (10 points)")
(newline)
(define q11_score (- 10 (run-tests test_exactly_one_q_v2 'verbose)))


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
(display "------Test Summary------\n")
(display "Q1 Scored: ")
(display q1_score)
(display "/10\n")
(display "Q2 Scored: ")
(display q2_score)
(display "/10\n")
(display "Q3 Scored: ")
(display q3_score)
(display "/5\n")
(display "Q4 Scored: ")
(display q4_score)
(display "/5\n")
(display "Q5 Scored: ")
(display q5_score)
(display "/10\n")
(display "Q6 Scored: ")
(display q6_score)
(display "/10\n")
(display "Q7 Scored: ")
(display q7_score)
(display "/10\n")
(display "Q8 Scored: ")
(display q8_score)
(display "/10\n")
(display "Q9 Scored: ")
(display q9_score)
(display "/10\n")
(display "Q10 Scored: ")
(display q10_score)
(display "/10\n")
(display "Q11 Scored: ")
(display q11_score)
(display "/10\n")

(define grand_total (+ q1_score q2_score q3_score q4_score q5_score q6_score q7_score q8_score q9_score q10_score q11_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")