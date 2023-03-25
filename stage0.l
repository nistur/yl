(eval (parse (read-file-text "src/stage0/stage0.l")))


(set! 'predicate-test (lambda (s)
			(cond
			  ((string? s) (println "string"))
			  ((val? s) (println "value")))))
(predicate-test "mushroom")
(predicate-test 1)
