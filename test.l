(println (cons 13 (cons 37 42)))

(set 'map
  (lambda (f lst)
    (if lst
        (cons (f (car lst))
              (map f (cdr lst)))
        lst)))

(set 'p (lambda (lst)
           (println "++" (car lst) (cdr lst) "--")))

(set 'l (cons 6 (cons 7 8)))
(set 'm (lambda (f lst)
           (if lst (p lst))
           (if lst
              (cons (f (car lst))
                 (m f (cdr lst)))
              lst ) ) )

(println (m (lambda (n) (+ n n)) l))

(println (map (lambda (n) (+ n n))
              (cons 3 (cons 4  5))))
