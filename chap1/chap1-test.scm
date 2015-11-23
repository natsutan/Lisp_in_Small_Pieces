(use gauche.test)
(test-start "my feature")

(load "./chap1.scm")

(test-section "group1")

(test* "sample" 1 (car '(1 2 3)))


;; atom?
(test-section "atom?")
(test* "atom?" #t (atom? 'foo))
(test* "atom?" #t (atom? 10))
(test* "atom?" #t (atom? "aaa"))
(test* "atom?" #t (atom? #\a))
(test* "atom?" #t (atom? #t))
(test* "atom?" #t (atom? #f))
(test* "atom?" #t (atom? '#(1 2 3)))

;; lookup
(test-section "kookup")
(test* "lookup" 3 (lookup 'x '((x . 3) (y . 10))))
(test* "lookup" 10 (lookup 'y '((x . 3) (y . 10))))
;(test* "lookup" 10 (lookup 'z '((x . 3) (y . 10))))

(define env-00 '((x . 3) (y . 10)))
(define env-01 '((x . 3) (y . 10)))
;; elvis
(test* "evlis" '(3 10 #f)  (evlis '(x y #f) env-00))

;; eprogn
(test* "eprogn" 5 (eprogn '(3 4 5) '()))
(test* "eprogn" 10 (eprogn '(x y) env-00))

;; update!
(test* "update" 2 (update2! 'x env-01 2))
(test* "update" '((x . 5) (y . 10)) (begin
                                      (update2! 'x env-01 5 )
                                      env-01))
(test* "update" '((x . 5) (y . 20)) (begin
                                      (update2! 'y env-01 20 )
                                      env-01))

;; invoke
(test* "invoke" 3 (invoke  (lambda (x) 3) '() ))
(test* "invoke" 8 (invoke  (lambda (x) (+ x 1)) 7))

;; extend2
(test* "extend2" (cons '(z . 20) env-00)
       (extend2 env-00 '(z) '(20) ))
(test* "extend2" (cons '(a . 12) (cons '(z . 20) env-00))
       (extend2 env-00 '(a z) '(12 20)))
                   
       


;; make-function
(test* "make-function" 3
       ((make-function '(x) '(3) env-00) '(2)))
(test* "make-function" 2
       ((make-function '(x) '(x) env-00) '(2)))

; evaluate
(test* "evaluate" 3 (evaluate 'x env-00))
(test* "evaluate" "aaa" (evaluate "aaa" '()))
(test* "evaluate" #\a (evaluate #\a '()))
(test* "evaluate" 10 (evaluate 10 '()))
(test* "evaluate" #t (evaluate #t '()))
(test* "evaluate" #f (evaluate #f '()))
(test* "evaluate" '#(1 2 3) (evaluate '#(1 2 3) '()))

(test* "evaluate" '(1 2) (evaluate ''(1 2)  '()))
(test* "evaluate" 3 (evaluate '(if #t x y) env-00))
(test* "evaluate" 10 (evaluate '(if #f x y) env-00))
(test* "evaluate" 5 (evaluate '(begin 5) env-00))
(test* "evaluate" 25 (evaluate '(begin x y 7 25) env-00))
(test* "evaluate" 0 (evaluate '(begin (if #t 3 5) 0) env-00))
(test* "evaluate" 2 (evaluate '(set! x 2)  env-01))
(test* "evaluate" 7 (begin
                     (evaluate '(set! x 7)  env-01)
                     (evaluate 'x  env-01)))
(test* "evaluate" 6 (evaluate
                     '((lambda (x) (+ x 2)) 4) env-01))






