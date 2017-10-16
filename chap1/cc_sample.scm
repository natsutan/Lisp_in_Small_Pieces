;; Programming Gauche Chapter19

(define (process/cc elt seed)
  (call/cc
   (lambda (cont)
     (print "found: " elt)
     (cont (cons elt seed)))))

(define (find-fold pred? proc seed lis)
  (cond [(null? lis) seed]
        [(pred? (car lis))
         (let ((seed2 (proc (car lis) seed)))
           (find-fold pred? proc seed2 (cdr lis)))]
        [else
         (find-fold pred? proc seed (cdr lis))]))

(find-fold odd? process/cc '() '(1 2 3 4 5 6 7 8 9 10))




