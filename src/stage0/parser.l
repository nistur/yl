;; Just a simple wrapper to parse a file into a format we
;; can work with
;; string -> ast
(set! 'parse-ast (lambda (file) (parse (read-file-text file))))
