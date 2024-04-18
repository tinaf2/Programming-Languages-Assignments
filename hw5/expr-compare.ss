#lang racket

(provide expr-compare)
(provide test-expr-compare)
(provide test-expr-v1)
(provide test-expr-v2)

(define LAMBDA (string->symbol "\u03BB"))

(define (append-vars v1 v2) (string->symbol (string-append (symbol->string v1) "!" (symbol->string v2))))

(define (comp-vars v1 v2) 
  (cond
  [(equal? v1 v2) v1]
  [(and (boolean? v1) (boolean? v2))
    (if v1 '% '(not %))]
  [else (list 'if '% v1 v2)])
)

(define (comp-hash-maps v1 v2 hashmapx hashmapy) (
  cond
    [(equal? (hash-ref hashmapx v1 v1) (hash-ref hashmapy v2 v2)) (hash-ref hashmapx v1 v1)]
    [else (let ([xMap (hash-ref hashmapx v1 v1)] [yMap (hash-ref hashmapy v2 v2)])  (comp-vars xMap yMap))]
  ))

(define (lambda-function v1 v2 hashmapx hashmapy) 
  (cond
    [(or (empty? v1) (empty? v2) (not (list? v1)) (not (list? v2)) (not (equal? (length v1) (length v2)))) (comp-hash-maps v1 v2 hashmapx hashmapy)]
    [(and (or (hash-has-key? hashmapx 'lambda) (hash-has-key? hashmapx LAMBDA)
         (hash-has-key? hashmapy 'lambda) (hash-has-key? hashmapy LAMBDA)) 
         (or (equal? (car v1) 'lambda) (equal? (car v1) LAMBDA) (equal? (car v2) 'lambda) (equal? (car v2) LAMBDA))) 
        (cons (comp-hash-maps (car v1) (car v2) hashmapx hashmapy) (lambda-function (cdr v1) (cdr v2) hashmapx hashmapy))]
    [(or (equal? (car v1) 'lambda) (equal? (car v1) LAMBDA) (equal? (car v2) 'lambda) (equal? (car v2) LAMBDA))
     (check-lambda v1 v2 hashmapx hashmapy)]
     [else (cons (lambda-function (car v1) (car v2) hashmapx hashmapy) (lambda-function (cdr v1) (cdr v2) hashmapx hashmapy))]
  )
)

(define (check-params v1 v2 hashmapx hashmapy) 
  (cond
    [(or (empty? v1) (empty? v2)) (comp-vars v1 v2)]
    [(not (equal? (hash-ref hashmapx (car v1) (car v1)) (hash-ref hashmapy (car v2) (car v2)))) (comp-vars v1 v2)]
    [else (cons (hash-ref hashmapx (car v1) (car v1)) (check-params (cdr v1) (cdr v2) hashmapx hashmapy))]
  ))

(define (v1-param v1 v2 hashmap)
  (cond
    [(or (empty? v1) (empty? v2)) hashmap]
    [(and (or (not (list? v1)) (not (list? v2))) (equal? v1 v2)) hashmap]
    [(and (or (not (list? v1)) (not (list? v2))) (not (equal? v1 v2))) 
      (hash-set hashmap v1 (append-vars v1 v2))]
    [(equal? (car v1) (car v2)) (define hashG (hash-set hashmap (car v1) (car v1)))
      (v1-param (cdr v1) (cdr v2) hashG)]
    [else (define hashG (hash-set hashmap (car v1) (append-vars (car v1) (car v2))))
      (v1-param (cdr v1) (cdr v2) hashG)]
  ))

(define (v2-param v1 v2 hashmap)
  (cond
    [(or (empty? v1) (empty? v2)) hashmap]
    [(and (or (not (list? v1)) (not (list? v2))) (equal? v1 v2)) hashmap]
    [(and (or (not (list? v1)) (not (list? v2))) (not (equal? v1 v2))) 
      (hash-set hashmap v2 (append-vars v1 v2))]
    [(equal? (car v1) (car v2)) (define hashG (hash-set hashmap (car v2) (car v2)))
      (v2-param (cdr v1) (cdr v2) hashG)]
    [else (define hashG (hash-set hashmap (car v2) (append-vars (car v1) (car v2))))
      (v2-param (cdr v1) (cdr v2) hashG)]
  ))

(define (check-lambda v1 v2 hashmapx hashmapy) 
	(cond
    [(or (empty? (cdr v1)) (empty? (cdr v2))) (list (expr-compare (car v1) (car v2)))]
    [(or (not (or (equal? (car v1) 'lambda) (equal? (car v1) LAMBDA))) (not (or (equal? (car v2) 'lambda) (equal? (car v2) LAMBDA)))) (comp-vars v1 v2)]
		[(not (equal? (length (cdr v1)) (length (cdr v2)))) (comp-vars v1 v2)]
    [(not (equal? (length (cadr v1)) (length (cadr v2)))) (comp-vars v1 v2)]
    [else (if (or (equal? (car v1) LAMBDA) (equal? (car v2) LAMBDA))
      (cons LAMBDA (cons (check-params (cadr v1) (cadr v2) (v1-param (cadr v1) (cadr v2) hashmapx) (v2-param (cadr v1) (cadr v2) hashmapy)) (lambda-function (cddr v1) (cddr v2) (v1-param (cadr v1) (cadr v2) hashmapx) (v2-param (cadr v1) (cadr v2) hashmapy))))
      (cons 'lambda (cons (check-params (cadr v1) (cadr v2) (v1-param (cadr v1) (cadr v2) hashmapx) (v2-param (cadr v1) (cadr v2) hashmapy)) (lambda-function (cddr v1) (cddr v2) (v1-param (cadr v1) (cadr v2) hashmapx) (v2-param (cadr v1) (cadr v2) hashmapy)))))]
	)
)

(define (expr-compare v1 v2) 
  (cond
	[(equal? v1 v2) v1]
	[(and (boolean? v1) (boolean? v2))
		(if v1 '% '(not %))]
	[(or (not (list? v1))
	     (not (list? v2)))
		(list 'if '% v1 v2)]
  [(or (empty? v1) (empty? v2)) (comp-vars v1 v2)]
  [(not (equal? (length v1) (length v2)) ) (list 'if '% v1 v2)]
  [(or (equal? (car v1) 'quote)  (equal? (car v2) 'quote)) (list 'if '% v1 v2)]
  [(or (equal? (car v1) 'let)  (equal? (car v2) 'let)) (list 'if '% v1 v2)]
  [(not (equal? (equal? (car v1) 'if)  (equal? (car v2) 'if))) (list 'if '% v1 v2)]
  [(or (equal? (car v1) 'lambda) (equal? (car v1) LAMBDA) (equal? (car v2) 'lambda) (equal? (car v2) LAMBDA))
    (define myHashx (hash-set (hash) 'INIT 'INIT))
    (define myHashy (hash-set (hash) 'INIT 'INIT))
     (check-lambda v1 v2 myHashx myHashy)]
	[else (cons (expr-compare (car v1) (car v2)) (expr-compare (cdr v1) (cdr v2)))]
    )
)


;; test-expr-compare

(define (test-expr-compare v1 v2)
  (and (equal? (eval (list 'let '([% #t]) (expr-compare v1 v2))) (eval v1))
       (equal? (eval (list 'let '([% #f]) (expr-compare v1 v2))) (eval v2))))

;; test-expr

(define test-expr-v1 '(if (equal? ((lambda (x y) (* x y)) 2 3) (/ (* 6 4) 2)) (+ 5 ((lambda (a b) (+ a b)) 1 2)) 100))
(define test-expr-v2 '(if (equal? ((lambda (a b) (* a b)) 3 4) (* (* 3 5) 4)) (+ 7 ((lambda (c e) (+ c e)) 1 2)) 12))





