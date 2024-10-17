#lang racket
(require rackunit)
(require rackunit/text-ui)

;CS 270
;Homework 7
;Professor B. Char, M. Boady,  J. Johnson, and G. Long

;Important Rules:
;1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
;2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
;    Recursive helper functions are allowed (the main function not being recursive).
;3.) You may not use the set! command. If used, your answer will get a zero.
;4.) Using If/Cond to explicitly pass tests instead of following the instructions
;    will always result in a zero for that question.

;Each of the below questions has two parts.
;First, you will be asked to write a Racket function to solve a problem.
;Secondly, you will be asked to prove by induction that your
;Racket code has some property.


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 1a (10 points)
;Write a recursive function to compute
;the sum( 6*x^2 , x = 1..n) for a given n
;You must write a recursive function.
;If you use any iterative commands (for/loop/sum/etc you will receive a 0)

; Computes sum( 6*x^2 , x = 1..n)
; Input:  n an integer >= 1
; Output: an integer, the result of the summation
;Question 1
(define (spec_sum n)
  (if (equal? n 1)
      6
      (+ (* 6 (* n n)) (spec_sum (- n 1)))))

;end

;Test Bed
(display "Question 1a spec_sum Tests (5 points)\n")
(define-test-suite test_spec_sum
  (check-equal? (spec_sum 1) 6)
  (check-equal? (spec_sum 2) 30)
  (check-equal? (spec_sum 3) 84)
  (check-equal? (spec_sum 4) 180)
  (check-equal? (spec_sum 5) 330)
  (check-equal? (spec_sum 6) 546)
  (check-equal? (spec_sum 7) 840)
  (check-equal? (spec_sum 8) 1224)
  (check-equal? (spec_sum 9) 1710)
  (check-equal? (spec_sum 10) 2310)
)
(define q1a_score (- 10 (run-tests test_spec_sum 'verbose)))

;Question 1b (10 points)
;Prove by induction that
;for all integers n >= 1 -> (spec_sum n) = 2n^3+3n^2+n

;Give your proof below in comments

;We are anchored at n = 1
;Base Case
;LHS:
;1 (spec_sum 1); Premise of Base Case RHS
;2 (if (equal? 1 1) 6 (+ (* 6 (* 1 1)) (spec_sum (- 1 1)))); Apply Def of spec_sum
;3 (if #t 6 (+ (* 6 (* 1 1)) (spec_sum (- 1 1)))); Evaluate equal?
;4 6; Evaluate if

;RHS:
;1 2(1)^3 + 3(1)^2 + 1; Premise of Base Case RHS
;2 2 + 3(1)^2 + 1; Compute Exponent
;3 2+3+1; Compute exponent
;4 5+1; Compute addition
;5 6; Compute addition

;Since LHS = RHS( both 6) base case is established.

;Inductive Hypothesis:
 ; For all integers k >= 1, (spec_sum k) = 2k^3+3k^2+k

 ;Leap Case:
;LHS:
;1 (spec_sum (+ k 1)); Premise of Leap Case LHS
;2(if(equal?(+k1)1)6(+(*6(*(+k1)(+k1)))(spec_sum(-(+k1)1))));ApplyDefofspec_sum
;3(if#f6(+(*6(*(+k1)(+k1)))(spec_sum(-(+k1)1))));Evaluateequal?
;4(+(*6(*(+k1)(+k1)))(spec_sum(-(+k1)1)));Evaluateif
;5 (+ (* 6 (* (k+1) (+ k 1))) (spec_sum (- (+ k 1) 1))); Compute addition
;6 (+ (* 6 (* (k+1) (k+1))) (spec_sum (- (+ k 1) 1))); Compute addition
;7 (+ (* 6 (k+1)^2) (spec_sum (- (+ k 1) 1))); Compute multiplication
 ;8 (+ (6(k+1)^2) (spec_sum (- (+ k 1) 1))); Compute multiplication
;9 (+ (6(k+1)^2) (spec_sum (- (k+1) 1))); Compute addition
;10 (+ (6(k+1)^2) (spec_sum k)); Compute subtraction
 ;11 (+ (6(k+1)^2) (2k^3+3k^2+k)); By IH
;12 2k^3 + 3k^2 + k + 6(k+1)^2; Compute addition
;13 2k^3 + 3k^2 + k + 6(k^2 + 2k + 1); Compute (a+b)^2
;14 2k^3 + 3k^2 + k + 6k^2 + 12k + 6; Compute multiplication
 ;15 2k^3 + 9k^2 + k +12k +6; Compute addition
;16 2k^3 + 9k^2 + 13k +6; Compute addition

;RHS:
 ;1 2(k+1)^3 + 3(k+1)^2 + k + 1; Premise of Leap Case RHS
;2 2(k^3+3k^2+3k+1)+3(k+1)^2+k+1;Compute(a+b)^3
;3 2k^3 + 6k^2 + 6k + 2 + 3(k+1)^2 + k + 1; Compute multiplication
;4 2k^3+6k^2+6k +2+3(k^2+2k+1)+k+1;Compute(a+b)^2
 ;5 2k^3+6k^2+6k +2+3k^2+6k+3+k+1;Computemultiplication
;6 2k^3+9k^2+6k+2+6k+3+k+1;Computeaddition
 ;7 2k^3 + 9k^2 + 13k + 2 + 3 + 1;Compute addition
 ;8 2k^3 + 9k^2 + 13k + 6; Compute addition

;Since LHS = RHS (both 2k^3 + 9k^2 + 13k + 6) leap case is established 
 ;The Leap Case and Base Case are established thus by POMI,
;For all integers n >= 1 -> (spec_sum n) = 2n^3+3n^2+n

;Question 2 (10 points)
; Write a recursive function evenzeros to check if a list of integers
; contains an even number of zeros.

; Input:  L is a list of integers.
; Output: a boolean value which is true when an even number of the elements
;          in L is equal to zero and false otherwise.
; Requirements: if you are actually counting the number of zeroes, you are
; implementing this in a bad way.  There should be NO arithmetic involved.

;Question 2
(define (evenzeros X)
  (if (null? X)
      #t
      (if (equal? (first X) 0)
          (not (evenzeros (rest X)))
          (evenzeros (rest X)))))

;end
;Test Bed
(display "Question 2a evenzeros Tests (10 points)\n")
(define-test-suite test_even_zeros
  (check-equal? (evenzeros '()) #t)
  (check-equal? (evenzeros '(1)) #t)
  (check-equal? (evenzeros '(0)) #f)
  (check-equal? (evenzeros '(0 0)) #t)
  (check-equal? (evenzeros '(7 0)) #f)
  (check-equal? (evenzeros '(1 -2)) #t)
  (check-equal? (evenzeros '(0 0 1)) #t)
  (check-equal? (evenzeros '(4 0 1)) #f)
  (check-equal? (evenzeros '(1 0 8)) #f)
  (check-equal? (evenzeros '(0 11 0 -9)) #t)
)
(define q2a_score (- 10 (run-tests test_even_zeros 'verbose)))
;Question 2b (10 points)
;Prove by induction, algebra, and equational reasoning that
;If L contains an even number of zeros then (equal? (evenzeros L) #t)
;If L contains an odd number of zeros then (equal? (evenzeros L) #f)
;Hint:
;Assume two different list E with an even number and O with an odd number
;You need 4 cases (cons 0 E), (cons x E), (cons 0 O), (cons x O)
;Where, x!=0, E is a list with an even number of zeros and O is a list
;with an odd number of zeros.

;We are anchored at X is null 
 ;Base Case:
 ;LHS:
 ;1 (evenzeros null) ; Premise of Base Case LHS
 ;2 (if (null? null) #t (if (equal? (first null) 0) (not (evenzeros (rest null))) (evenzeros (rest null)))); Apply Def of evenzeros
 ;3 (if #t #t (if (equal? (first null) 0) (not (evenzeros (rest null))) (evenzeros (rest null)))); Evaluate null?
 ;4 #t; Evaluate if

 ;RHS:
 ;1 #t; Premise of Base Case LHS

 ;Since LHS = RHS (both #t) the base case is established 185
 ;Inductive Hypothesis
 ;For all lists X with an even number of zeros (evenzeros X)=#t, For all lists Y with an odd number of zeros (evenzeros Y)=#f

 ;Leap Case 1:
 ;LHS:
 ;1 (evenzeros (cons 0 X)); Premise of Leap Case LHS
 ;2 (if (null? (cons 0 X)) #t (if (equal? (first (cons 0 X)) 0) (not (evenzeros (rest (cons 0 X)))) (evenzeros (rest (cons 0 X))))); Aplly Def of evenzeros
 ;3 (if #f #t (if (equal? (first (cons 0 X)) 0) (not (evenzeros (rest (cons 0 X)))) (evenzeros (rest (cons 0 X))))); Evalutate null?
 ;4 (if (equal? (first (cons 0 X)) 0) (not (evenzeros (rest (cons 0 X)))) (evenzeros (rest (cons 0 X)))); Evaluate if
 ;5 (if (equal? 0 0) (not (evenzeros (rest (cons 0 X)))) (evenzeros (rest (cons 0 X)))); Evaluate first
 ;6 (if #t (not (evenzeros (rest (cons 0 X)))) (evenzeros (rest (cons 0 X)))); Evaluate equal?
 ;7 (not (evenzeros (rest (cons 0 X)))); Evaluate if
 ;8 (not (evenzeros X)); Evaluate rest
 ;9 (not #t); By IH
  ;10 #f; Evaluate not 201
 ;RHS:
 ;1 #f; Premise Case of Leap Case RHS

 ; Since LHS = RHS (both #f) , Leap Case 1 is established 206
 ;Leap Case 2:
 ;LHS:
 ;1 (evenzeros (cons x E)); Premise of Leap Case LHS
 ;2 (if (null? (cons x E)) #t (if (= 0 (first (cons x E))) (not (evenzeros (rest (cons x E)))) (evenzeros (rest (cons x E))))); Apply def of evenzeros
 ;3 (if #f (if (= 0 (first (cons x E))) (not (evenzeros (rest (cons x E)))) (evenzeros (rest (cons x E)))); Evaluate null?
 ;4 (if (= 0 (first (cons x E))) (not (evenzeros (rest (cons x E)))) (evenzeros (rest (cons x E)))); Evaluate if
 ;5 (if (= 0 x) (not (evenzeros (rest (cons x E)))) (evenzeros (rest (cons x E)))); Evaluate first
 ;6 (if #f (not (evenzeros (rest (cons x E)))) (evenzeros (rest (cons x E)))); Evaluate =
 ;7 (evenzeros (rest (cons x E))); Evaluate if
 ;8 (evenzeros E)); Evaluate rest
 ;9#t;ByIH

 ;RHS:
 ;1 #t; Premise of Leap Case RHS

 ; Since LHS = RHS (both #t) , Leap Case 2 is established 223
 ;Leap Case 3:
 ;LHS:
 ;1 (evenzeros (cons 0 O)); Premis of Leap Case LHS
 ;2 (if (null? (cons 0 O)) #t (if (= 0 (first (cons 0 O))) (not (evenzeros (rest (cons 0 O)))) (evenzeros (rest (cons 0 O)))); Apply Def of evenzeros
 ;3 (if #f (if (= 0 (first (cons 0 O))) (not (evenzeros (rest (cons 0 O)))) (evenzeros (rest (cons 0 O)))); Evaluate null?
 ;4 (if (= 0 (first (cons 0 O))) (not (evenzeros (rest (cons 0 O)))) (evenzeros (rest (cons 0 O)))); Evaluate if
 ;5 (if (= 0 0) (not (evenzeros (rest (cons 0 O)))) (evenzeros (rest (cons 0 O)))); Evaluate first
 ;6 (if #t (not (evenzeros (rest (cons 0 O)))) (evenzeros (rest (cons 0 O)))); Evaluate =
 ;7 (not (evenzeros (rest (cons 0 O)))); Evaluate if
 ;8 (not (evenzeros O)); Evaluate rest
 ;9 (not #f); By IH
 ;10 #t; Evaluate not

 ;RHS:
 ;1 #t; Premise of Leap Case RHS

 ; Since LHS = RHS (both #t) , Leap Case 3 is established 241
 ;Leap Case 4:
 ;LHS:
 ;1 (evenzeros (cons x O)) ;Premise of Leap Case LHS
 ;2 (if (null? (cons x O)) #t (if (= 0 (first (cons x O))) (not (evenzeros (rest (cons x O)))) (evenzeros (rest (cons x O)))); Apply Def of evenzeros
 ;3 (if #f (if (= 0 (first (cons x O))) (not (evenzeros (rest (cons x O)))) (evenzeros (rest (cons x O)))); Evaluate null?
 ;4 (if (= 0 (first (cons x O))) (not (evenzeros (rest (cons x O)))) (evenzeros (rest (cons x O)))); Evaluate if
 ;5 (if (= 0 x) (not (evenzeros (rest (cons x O)))) (evenzeros (rest (cons x O)))); Evaluate first
 ;6 (if #f (not (evenzeros (rest (cons x O)))) (evenzeros (rest (cons x O)))); Evaluate =
 ;7 (evenzeros (rest (cons x O))); Evaluate if
 ;8 (evenzeros O); Evaluate rest
 ;9#f;ByIH

 ;RHS:
 ;1 #f; Premise of Leap Case RHS


 ; Since LHS = RHS (both #f) , Leap Case 4 is established 259
 ;Since Base Case and all the Leap Cases are established, thus by POMI,
 ;For all lists E with an even number of zeros (evenzeros E)=#t and For all lists O with an odd number of zeros (evenzeros O)=#f


;Q3a (10 Points)
;Write a recursive function duplicate that takes every element in a list
;and makes a second copy of the item.
;For example if we started with (1 2 3)
;then the duplicated list would be (1 1 2 2 3 3)


; Duplicates Elements in a list
; Input:  X a list
; Output: A new list with two copies of even value in X
;Question 3
(define (duplicate X)
  (if (null? X)
      null
      (cons (first X) (cons (first X) (duplicate (rest X))))))

;end
(display "Question 3a duplicate Tests (10 points)\n")
(define-test-suite test_duplicate
  (check-equal? (duplicate '()) '())
  (check-equal? (duplicate '(1)) '(1 1))
  (check-equal? (duplicate '(1 2)) '(1 1 2 2))
  (check-equal? (duplicate '(4 6)) '(4 4 6 6))
  (check-equal? (duplicate '((1) (2 3))) '((1) (1) (2 3) (2 3)))
  (check-equal? (duplicate '(4 5 6)) '(4 4 5 5 6 6))
  (check-equal? (duplicate '(7 8 9 10)) '(7 7 8 8 9 9 10 10))
  (check-equal? (duplicate '(1 2 3 4 5)) '(1 1 2 2 3 3 4 4 5 5))
  (check-equal? (duplicate '(9 9 9)) '(9 9 9 9 9 9))
  (check-equal? (duplicate '(1 4 5 6 4 3 4 5))
                '(1 1 4 4 5 5 6 6 4 4 3 3 4 4 5 5))
)
(define q3a_score (- 10 (run-tests test_duplicate 'verbose)))

;Q3b (10 Points)
;Prove By Induction
;(length (duplicate L)) = 2x where x is the length of list L
;You may use the following properties of length
;Length Property 1: (length null) = 0 
;Length Property 2: If a is any object and B is any list
;(length (cons a B)) = (+ 1 (length B))
;You may Justify lines by saying [By Length Property 1]
;Hint: An equals can be used both ways.


;We are anchored at X is null 310
 ;Base Case:
 ;LHS:
 ;1 (length (duplicate null)); Premise of Base Case LHS
 ;2 (length (if (null? null) null (cons (first null) (cons (first null) (duplicate (rest null)))))); Apply def of duplicate
 ;3 (length (if #t null (cons (first null) (cons (first null) (duplicate (rest null)))))); Evaluate null?
 ;4 (length null); Evaluate if
 ;5 0; Evaluate length

 ;RHS:
 ;1 2(0); Premise of Base Case RHS
 ;2 0; Compute multiplication

 ;Inductive Hypothesis:
 ; Supposed k is the number of elements in list X, for all list we have (length (duplicate X)) = 2k

 ;Leap Case:
 ;LHS:
 ;1 (length (duplicate (cons a X)); Premise of Leap Case LHS
 ;2 (length (if (null? (cons a X)) null (cons (first (cons a X)) (cons (first (cons a X)) (duplicate (rest (cons a X))))))); Apply Def of duplicate
 ;3 (length (if #f null (cons (first (cons a X)) (cons (first (cons a X)) (duplicate (rest (cons a X))))))); Evaluate null?
 ;4 (length (cons (first (cons a X)) (cons (first (cons a X)) (duplicate (rest (cons a X))))))); Evaluate if
 ;5 (length (cons a (cons (first (cons a X)) (duplicate (rest (cons a X))))))); Evaluate first
 ;6 (length (cons a (cons a (duplicate (rest (cons a X))))))); Evaluate first
 ;7 (length (cons a (cons a (dupicate X)))); Evaluate rest
 ;8 (+ 1 (length (cons a (duplicate X)))); By Lemma 2
 ;9 (+ 1 (+ 1 (length (duplicate X))); By Lemma 2
 ;10(+1(+12k));ByIH
 ;11 (+ 1 (2k+1)); Evaluate +
 ;12 2k+2; Evaluate +

 ;RHS:
 ;1 2(k+1); Premise of Leap Case LHS
 ;2 2k+2; Compute multiplication

 ;Since LHS = RHS (both 2k + 2) Leap Case is established.

 ;The Leap Case and Base Case are established thus by POMI,
 ; supposed x is the number of elements in L, for all lists we have (length (duplicate L)) = 2x


;Question 4a (10pts)
;Write a recursive function (cut_end L) that removes the last element from the list

; Removes the last element in a list
; Input:  X non-empty a list
; Output: A new list with the last element removed
;Question 4
(define (cut_end L)
 (if (null? (rest L))
     null
     (cons (first L) (cut_end (rest L)))))

;end

(display "Question 4a cut_end Tests (10 points)\n")
(define-test-suite test_cut_end
  (check-equal? (cut_end '(1)) '())
  (check-equal? (cut_end '(1 2)) '(1))
  (check-equal? (cut_end '(3 4 5)) '(3 4))
  (check-equal? (cut_end '( (1) (2) (3) )) '( (1) (2) ))
  (check-equal? (cut_end '((1 2 3 4))) '())
  (check-equal? (cut_end '((1 2) (3 4))) '((1 2)))
  (check-equal? (cut_end '(9 9 8)) '(9 9))
  (check-equal? (cut_end '(AND A B)) '(AND A))
  (check-equal? (cut_end '(NOT X)) '(NOT))
)
(define q4a_score (- 10 (run-tests test_cut_end 'verbose)))

;Question 4b
 ;Supposed x represents the number of elements in L
 ;Prove by Induction that
 ;For all lists with length >= 1 we have (length (cut_end L)) = x-1
 ;You may use the properties of length from Question 3

;1 (length (cut_end null)); Premise of Base Case LHS
 ;2 (length (if (null? null) null (cons (first null) (cut_end (rest null))))); Apply def of cut_end
 ;3 (length (if #t null (cons (first null) (cut_end (rest null))))); Evaluate null?
 ;4 (length null); Evaluate if
;5 0; Evaluate length

 ;RHS:
 ;1 2(0); Premise of Base Case RHS
;2 0; Compute multiplication

;Inductive Hypothesis:
 ; Supposed k is the number of elements in list X, for all lists with length >= 1 we have (length (cut_end X)) = k-1

 ;Leap Case:
 ;LHS:
 ;1 (length (cut_end (cons a X)); Premise of Leap Case LHS
 ;2 (length (if (null? (cons a X)) null (cons (first (cons a X)) (cut_end (rest (cons a X)))))); Apply Def of cut_end

 ;3 (length (if #f null (cons (first (cons a X)) (cut_end (rest (cons a X)))))); Evaluate null?
 ;4 (length (cons (first (cons a X)) (cut_end (rest (cons a X))))); Evaluate if
 ;5 (length (cons a (cut_end (rest (cons a X))))); Evaluate first
 ;6 (length (cons a (cut_end X))); Evaluate rest
 ;7 (+ 1 (length (cut_end X))); By Lemma 2
 ;8(+1(-k1));ByIH
 ;9 (+ 1 (k-1)); Compute subtraction
 ;10 k; Compute addition

 ;RHS:
 ;1 (k+1) - 1; Premise of Leap Case LHS
 ;2 k; Compute subtraction

 ;Since LHS = RHS (both k) Leap Case is established. 422
 ;The Leap Case and Base Case are established thus by POMI,
 ; supposed x represents the number of elements in L, for all lists with length >= 1 we have (length (cut_end L)) = x-1

;Question 5a (10pts)
;Write a recursive function (add_pairs L)
;that adds pairs of numbers.
;You may assume the length of L will always be even.

; Adds pairs of numbers
; Input:  L a list (the list must have even length)
; Output: A new list with pairs of elements added together.
;Question 5
(define (add_pairs L)
  (if (null? L)
      null
      (cons (+ (first L) (first (rest L))) (add_pairs (rest (rest L))))))

;end

(display "Question 5a add_pairs Tests (10 points)\n")
(define-test-suite test_add_pairs
  (check-equal? (add_pairs '()) '())
  (check-equal? (add_pairs '(1 2)) '(3))
  (check-equal? (add_pairs '(1 2 3 4)) '(3 7))
  (check-equal? (add_pairs '(2 2 2 2)) '(4 4))
  (check-equal? (add_pairs '(0 -1 -2 3)) '(-1 1))
  (check-equal? (add_pairs '(1 1 1 1)) '(2 2))
  (check-equal? (add_pairs '(1 2 3 4 5 6 7 8)) '(3 7 11 15))
  (check-equal? (add_pairs '(9 9 9 9 9 9)) '(18 18 18))
  (check-equal? (add_pairs '(7 3 4 6 5 5)) '(10 10 10))
  (check-equal? (add_pairs '(-9 9 -8 8)) '(0 0))
  
)
(define q5a_score (- 10 (run-tests test_add_pairs 'verbose)))

;Question 5b
;Prove by Induction that (length (add_pairs L)) = x/2, where x is the length of L
;for all L where (even? (length L)) and x = (length L) >= 0
;You may use the properties of length from Question 3

;We are anchored at X is null

;Base Case:
 ;LHS:
 ;1 (length (add_pairs null)); Premise of Base Case LHS
 ;2 (length (if (null? null) null (cons (+ (first null) (first (rest null))) (add_pairs (rest (rest null)))))))); Apply def of add_pairs
 ;3 (length (if #t null (cons (+ (first null) (first (rest null))) (add_pairs (rest (rest null)))))))); Evaluate null?
 ;4 (length null); Evaluate if
 ;5 0; Evaluate length

 ;RHS:
 ;1 2(0); Premise of Base Case RHS
 ;2 0; Compute multiplication

 ;Inductive Hypothesis:
 ; Supposed k is the number of elements in list X, for all lists of even length it is true that (length (add_pairs X)) = k/2

 ;Leap Case:
 ;LHS:
 ;1 (length (add_pairs (cons a X)); Premise of Leap Case LHS
 ;2 (length (if (null? (cons a X)) null (cons (+ (first (cons a X)) (first (rest (cons a X)))) (add_pairs (rest (rest (cons a X))))))))); Apply Def of add_pairs
 ;3 (length (if #f null (cons (+ (first (cons a X)) (first (rest (cons a X)))) (add_pairs (rest (rest (cons a X))))))))); Evaluate null?
 ;4 (length (cons (+ (first (cons a X)) (first (rest (cons a X)))) (add_pairs (rest (rest (cons a X)))))))); Evaluate if
 ;5 (+ 1 (length (add_pairs (rest (rest (cons a X)))))); By Lemma 2
 ;6 (+ 1 (length (add_pairs (rest X)))); Evaluate rest
 ;7 (+ 1 ((k-1)/2)); By IH
 ;8 (k+1)/2; Compute addition

 ;RHS:
 ;1 (k+1)/2; Premise of Leap Case LHS

 ;Since LHS = RHS (both k+1/2) Leap Case is established. 499
 ;The Leap Case and Base Case are established thus by POMI,
 ; Suppose n represents the number of elements in L, for all lists of even length it is true that (length (add_pairs L)) = n/2


;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1a Scored: ")
(display q1a_score)
(display "/10\n")
(display "Q1b Scored: ?/10 (Graded by TA)\n")
(display "Q2a Scored: ")
(display q2a_score)
(display "/10\n")
(display "Q2b Scored: ?/10 (Graded by TA)\n")
(display "Q3a Scored: ")
(display q3a_score)
(display "/10\n")
(display "Q3b Scored: ?/10 (Graded by TA)\n")
(display "Q4a Scored: ")
(display q4a_score)
(display "/10\n")
(display "Q4b Scored: ?/10 (Graded by TA)\n")
(display "Q5a Scored: ")
(display q5a_score)
(display "/10\n")
(display "Q5b Scored: ?/10 (Graded by TA)\n")


(define grand_total (+ q1a_score q2a_score q3a_score q4a_score q5a_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")