

(define variables-table (make-hash-table))


(hash-table-push! variables-table + 3)
(ref variables-table +)


(define lookup3
  (lambda (id env)
    (ref variables-table id)))

(hash-table-push! variables-table + 3)
(lookup3 + '())
