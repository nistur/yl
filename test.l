(println (cons 13 (cons 37 42)))

(set 'map
  (lambda (f lst)
    (if lst
        (cons (f (car lst))
              (map f (cdr lst)))
        lst)))

(println (map (lambda (n) (+ n n))
              (cons 3 (cons 4 (cons 5)))))
