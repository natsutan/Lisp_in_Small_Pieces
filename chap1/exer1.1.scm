(load "./chap1.scm")


(chap1-scheme-bat '((+ 3 5)))

(chap1-scheme-bat
 '((set! pow (lambda (x) (* x x)))
   (pow 5)))

(trace-on)
(trace 'fact)


(chap1-scheme-bat
 '((set! fact
         (lambda (x)
           (if (= x 1)
               1
               (* (fact (- x 1)) x))))
   (fact 5)))


