;; define apply

(apply max '(1 2 3))  ; => d3
(apply + 1 2 '(3 4 5)) ; => 15
(apply - 100 '(5 12 17)) ; => 66


(load "./chap1.scm")

(chap1-scheme-bat '((set! max (lambda (l) (* x x)))
                    (max 1 2 3)))

(chap1-scheme-bat '((max 1 2 3)))



(define my_max
  (lambda (f . s)
    (format #t "f=~A, s=~A " f s)
    (format #t "~A~%" (length s))
    (when (= (length s) 1) s)
    (when (= (length s) 2)
      (if (> s t)
          s
          t))
    (my_max (cdr s))))
    


(define max
  (lambda x
    (do ((x (cdr x) (cdr x))
         (r (car x)
            (if (> (car x) r) (car x) r)))
        ((null? x) r))))        
            

(define (my-max a . b)
  (define (iter b)
    (if (null? (cdr b)) (car b)
        (if (> (car b) (cadr b))
            (iter (cons (car b) (cddr b)))
            (iter (cdr b)))))
  (let ((c (iter b)))
    (if (> a c) a c)))


(my-max 1 2)
(my-max 1 2 3)

(pair? '(2 3))

(length+ '((2)))

