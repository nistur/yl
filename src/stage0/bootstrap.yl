(set! 'eval--internal eval)
(set! 'eval (lambda (x) (eval--internal (parse x))))
(set! 'stage0-path "src/stage0/")
(set! 'load (lambda (file) (eval (read-file-text (concat stage0-path file)))))
