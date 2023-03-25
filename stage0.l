(eval (parse (read-file-text "src/stage0/stage0.l")))

(let ((stage1 (car (cdr args))))
   (print-cell (parse-ast  stage1)))
