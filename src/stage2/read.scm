(import (scheme base)
        (scheme char)
        (scheme write)
        (scheme file)
        (scheme case-lambda))

;; sketch, not currently used
(define-record-type <syntax-object>
  (make-syntax-object datum srcloc context)
  syntax-object?
  (datum syntax-object-datum)
  (srcloc syntax-object-srcloc)
  (context syntax-object-context))

(define yl-read
  (case-lambda
    (() (yl-read (current-input-port)))
    ((in-port) (yl-read-from-port in-port))))

(define (call-with-peeked-char f prt)
  (f (peek-char prt)))

(define-syntax with-peeked-char
  (syntax-rules ()
    ((_ cvar prt body0 body ...)
     (let ((p prt))
       (call-with-peeked-char
        (lambda (cvar) body0 body ...)
        p)))))

(define (yl-read-from-port prt)
  (yl-read-through-whitespace prt)
  (with-peeked-char c prt
    (cond
     ((eof-object? c) (eof-object))
     ((number-prefix-char? c) (read-symbol-or-prefixed-number prt))
     ((char-starts-list? c)
        (read-char prt)
        (yl-read-list-elements prt))
     (else
      (read-symbol-from-port prt)))))

(define (yl-read-list-elements prt)
  (yl-read-through-whitespace prt)
  (with-peeked-char c prt
    (cond
     ((eof-object? c) (error "unexpected eof"))
     ((char-ends-list? c) (begin (read-char prt) '()))
     (else (let ((elt (yl-read-from-port prt)))
             (cons elt (yl-read-list-elements prt)))))))

(define (read-symbol-or-prefixed-number prt)
  (define prefix-char (read-char prt))
  (with-peeked-char c prt
    (define-values (conv end-pred?) 
      (if (char-numeric? c)
          (values string->number
                  char-ends-number?)
          (values string->symbol
                  char-ends-symbol?)))
    (conv (string-append (char->string prefix-char)
                         (read-from-port-until end-pred? prt)))))

(define (make-char-=?-fn a)
  (lambda (b) (char=? a b)))

(define char-starts-list? (make-char-=?-fn #\())
(define char-ends-list? (make-char-=?-fn #\)))
(define char-starts-string? (make-char-=?-fn #\"))
(define quote-char? (make-char-=?-fn #\'))

(define (number-prefix-char? c)
  (or (char=? #\+ c)
      (char=? #\- c)))

(define (read-from-port-until pred? prt)
  (define (inner prt)
    (with-peeked-char c prt
      (if (or (eof-object? c)
              (pred? c))
          '()
          (let ((read-c (read-char prt)))
            (cons read-c (inner prt))))))
  (list->string (inner prt)))

(define (read-from-port-while pred? prt)
  (read-from-port-until (lambda (x) (not (pred? x))) prt))

(define (read-symbol-from-port prt)
  (string->symbol (read-from-port-until char-ends-symbol? prt)))

(define (read-number-from-port prt)
  (string->number (read-from-port-until char-ends-number? prt)))

(define (char-ends-symbol? c)
  ;; incomplete
  (or (char-whitespace? c)
      (char-starts-string? c)
      (quote-char? c)
      (char-starts-list? c)
      (char-ends-list? c)))

(define (char-ends-number? c)
  ;; incomplete
  (or (char-whitespace? c)
      (char-starts-string? c)
      (quote-char? c)
      (char-starts-list? c)
      (char-ends-list? c)))

(define (yl-read-through-whitespace prt)
  (read-from-port-while char-whitespace? prt))

(define (char->string c)
  (vector->string (vector c)))

(let ((prt
       ;(open-input-string "   (  aff \n \tbaff  mm a fo) ooo")
       (open-input-string "  -123 +321 -QED-  (  aff \n \tbaff  mm a fo) ooo")

       ))
  (display (yl-read prt))
  (newline)
  (display "'")
  (display (peek-char prt))
  (display "'")
  (newline)
  (display (yl-read prt))
  (newline)
  (display (yl-read prt))
  (newline)
  (display (eof-object? (peek-char prt)))
  (close-input-port prt))

;(define test-file (open-input-file "test.scm"))
;(close-port test-file)
