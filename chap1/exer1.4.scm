(use srfi-1)

; env.global
; evaluate
(define env.init '())

(define env.global env.init)
(define *trace-en* #f)
(define *trace-list* '())
(define *variable-table* (make-hash-table))

(define (chap1-scheme)
  (define (toplevel)
    (display (evaluate (read) env.global))
    (toplevel))
  (toplevel))


(define evaluate
  (lambda (e env)
    
    (if (atom? e)
        (cond
         [(symbol? e) (car (ref *variable-table* e))]
         [(or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e]
         [else (wrong "cannot evaluate" e)])
        (case (car e)
          [(quote) (cadr e)]
          [(if) (if (evaluate (cadr e) env)
                    (evaluate (caddr e) env)
                    (evaluate (cadddr e) env))]
          [(begin) (eprogn (cdr e) env)]
          [(set!) (let ([id (cadr e)]
                        [value (evaluate (caddr e) env)])
                    (hash-table-pop! *variable-table* id)
                    (hash-table-push! *variable-table* id value)
                    value)]
          [(lambda) (s.make-function (cadr e) (cddr e) env)]
          [else
           (begin
             (let ([func (car e)]
                   [args (evlis (cdr e) env)])
               (when (trace? func) (format #t "call ~A ~A~%" func args))
               (let ([result (invoke (evaluate func env) args)])
                 (when (trace? func) (format #t "rerurn ~A ~A result ~A ~%" func args result))
                 result)))]
          ))))

(define atom?
  (lambda (e)
    (or (symbol? e) (number? e) (string? e) (char? e) (boolean? e) (vector? e))))

(define lookup
  (lambda (id env)
    (if (pair? env)
        (if (eq? (caar env) id)
            (cdar env)
            (lookup id (cdr env)))
        (wrong "No such binding" id))))

(define wrong
  (lambda (msg e)
    (print "My scheme error")
    (print msg)
    (print e)
    (error "myapp error"  :debug-info msg :reason e)))

(define evlis
  (lambda (exps env)
    (if (pair? exps)
        (let ((arg1 (evaluate (car exps) env)))
          (cons arg1 (evlis (cdr exps) env)))
        '())))

(define eprogn
  (lambda (exps env)
    (if (pair? exps)
        (if (pair? (cdr exps))
            (begin (evaluate (car exps) env)
                   (eprogn (cdr exps) env))
            (evaluate (car exps) env))

        empty-begin)))

(define empty-begin 813)
               
(define update2!
  (lambda (id env value)
    (if (pair? env)
        (if (eq? (caar env) id)
            (begin
              (set-cdr! (car env) value)
              value)
            (begin
              (update2! id (cdr env) value)))            
        (wrong "No match binding" id))))

(define invoke
  (lambda (fn args)
    (if (procedure? fn)
        (fn args)
        (wrong "Not function" fn))))

(define make-function
  (lambda (variables body env)
    (lambda (values)
      (eprogn body (extend2 env.global variables values)))))

(define extend2
  (lambda (env variables values)
    (cond ((pair? variables)
           (if (pair? values)
               (cons (cons (car variables) (car values))
                     (extend2 env (cdr variables) (cdr values)))
               (wrong "too less values" values)))
           ((null? variables)
            (if (null? values)
                env
                (wrong "too much values" values)))
           ((symbol? variables) (cons (cons variables values) env)))))


(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (begin 
       (hash-table-push! *variable-table* 'name 'void)
       'name))
    ((definitial name value)
     (begin
       (hash-table-push! *variable-table* 'name value)
       'name))))

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name
       (lambda (values)
         (if (= arity (length values))
             (apply value values) ; the real apply of scheme
             (wrong "incorrect arity" (list 'name valeus))))))))


;; trace exer 1.1
(define trace-on
  (lambda ()
    (set! *trace-en* #t)))

(define trace-off
  (lambda ()
    (set! *trace-en* #f)))

(define trace
  (lambda (s)
    (set! *trace-list* (cons s *trace-list*))))

(define trace?
  (lambda (s)
    (and *trace-en*
         (member s *trace-list*))))


(define chap1-scheme-bat
  (lambda (args)
    (dolist (s args)
            (format #t "YUKI.N> ~A~%" (evaluate s env.global)))))

;;; initial
(definitial x 30)
(definitial foo)
(definitial bar)
(definitial fib)
(definitial fact)
(definitial pow)

(defprimitive cons cons 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive * * 2)
(defprimitive / / 2)
(defprimitive = = 2)

;;; shallow binidng
(define (s.make-function variables body env)
  (lambda (values)
    (map (lambda (var val) (hash-table-push! *variable-table* var val)) variables values)
    (let ((result (eprogn body '())))
      (dolist (val variables)
              (hash-table-pop! *variable-table* val))
      result)))

;;;;; test

(chap1-scheme-bat '((+ 3 5)))

 (chap1-scheme-bat
  '((set! pow (lambda (x) (* x x)))
   (pow 5)))


 (chap1-scheme-bat
  '((set! fact
          (lambda (x)
            (if (= x 1)
                1
                (* (fact (- x 1)) x))))
    (fact 5)))





    

                 
