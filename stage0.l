(eval (parse (read-file-text "src/stage0/stage0.l")))

(let ((FILE (car (cdr args))))
   (eval (read-file-text FILE)))
