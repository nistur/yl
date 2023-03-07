
(set! 'cddr (lambda (x) (cdr (cdr x))))
(set! 'test-cons (cons 3 (cons 4 (cons 5 nil))))

(println "hello world")
(println (cons 1 2))
(println (cons 1 (cons 2 3)))
(println (car (cons 1 (cons 2 3))))
(println (cdr (cons 1 (cons 2 3))))
(println test-cons)

(println '(1 2 3))
(println (cddr (cons 1 (cons 2 3))))
