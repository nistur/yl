(eval (parse (read-file-text "src/stage0/stage0.l")))

(let ((test (concat stage0-path "banana"))
      (test2 (concat stage0-path "mushroom")))
   (println test)
   (println test2) )
