(set! 'lambda-test (lambda (a) (lambda (b) (println (+ a b)))))

((lambda-test 1) 2)
