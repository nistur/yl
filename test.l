(print-cell ast)

(set! 'cddr (lambda (x) (cdr (cdr x))))
(set! 'test-list-1 (cons 3 (cons 4 (cons 5 nil))))


(println "hello world")
(println (cons 1 2))
(println (cons 1 (cons 2 3)))
(println (car (cons 1 (cons 2 3))))
(println (cdr (cons 1 (cons 2 3))))
(println (cdr (cdr (cons 1 (cons 2 3)))))
(println test-list-1)
(println (cons 1 (cons 2 nil)))
(println (car (cons 1 (cons 2 nil))))
(println (cdr (cons 1 (cons 2 nil))))
(println (cdr (cons "foo" (cons "bar" "baz"))))
(println (cdr (cons "foo" (cons "bar" nil))))
(println '(1 2 3))
(println (car '(1 2 3)))
(println (cdr '(1 2 3)))
(println (cdr (cdr '(1 2 3))))

(println (cddr (cons 1 (cons 2 3))))

