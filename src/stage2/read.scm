;; Copyright (c) 2023 Philipp Geyer and Isak Andersson
;;
;;   This file is part of yl.
;;
;;   yl is free software: you can redistribute it and/or modify it
;;   under the terms of the GNU Lesser General Public License as
;;   published by the Free Software Foundation, either version 3 of
;;   the License, or (at your option) any later version.
;;
;;   yl is distributed in the hope that it will be useful, but WITHOUT
;;   ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;   Lesser General Public License for more details.
;;
;;   You should have received a copy of the GNU Lesser General Public
;;   License along with yl. If not, see
;;   <https://www.gnu.org/licenses/>.
;;
;; Philipp Geyer             Isak Andersson
;; philipp@geyer.co.uk       contact@bitpuffin.com

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
     ((char-numeric? c) (read-number-from-port prt))
     ((number-prefix-char? c) (read-symbol-or-prefixed-number prt))
     ((hash-char? c) (read-hash-element prt))
     ((char-starts-list? c)
        (read-char prt)
        (yl-read-list-elements prt))
     ((char-starts-string? c)
      (read-char prt)
      (read-string-from-port prt))
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

(define (read-string-from-port prt)
  (define (eof-err) (error "unexpected eof in the middle of string"))
  (define (read-intraline-whitespace)
    (with-peeked-char c prt
      (when (char-whitespace? c)
        (read-char prt)
        (read-intraline-whitespace prt))))
  (define (read-escape-sequence)
    (define c (read-char prt))
    (define escapes '((#\t . #\tab)
                      (#\n . #\newline)
                      (#\r . #\return)
                      (#\" . #\")
                      (#\\ . #\\)))
    (define escaped (assoc c escapes))
    (cond
     ((eof-object? c) (eof-err))
     ((char-whitespace? c) (read-intraline-whitespace))
     ((char=? #\x c) (error "NYI: hex scalar value characters"))
     (escaped (display (cdr escaped)))))
  (define (read-loop in-escape?)
    (if in-escape?
        (begin (read-escape-sequence) (read-loop #f))
        (let ((c (read-char prt)))
          (cond
           ((eof-object? c) (eof-err))
           ((char=? #\" c) (if #f #f)) ; void
           ((char=? #\\ c) (read-loop #t))
           ((char=? #\newline c) (display #\newline) (read-loop #f))
           (else (display c) (read-loop #f))))))
  (define out (open-output-string))
  (parameterize ((current-output-port out))
    (read-loop #f)
    (let ((str (get-output-string out)))
      (close-output-port out)
      str)))

(define (read-number-from-port prt)
  (string->number (read-from-port-until char-ends-number? prt)))

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

(define (read-hash-element prt)
  (define (bool-char? c) (or (char=? #\t c)
                             (char=? #\f c)))
  (define (bool-char->char c) (char=? #\t c))
  (define number-prefix-chars (string->list "eibodx"))
  (define (number-prefix? c) (member c number-prefix-chars))
  (define unsigned-vector-prefix? (make-char-=?-fn #\u))
  (define vector-start? (make-char-=?-fn #\())
  (define char-escape-prefix? (make-char-=?-fn #\\))

  (read-char prt) ; get rid of #
  (with-peeked-char c prt
    (cond
     ((bool-char? c) (bool-char->char (read-char prt)))
     ((number-prefix? c) (read-prefixed-number prt))
     ((unsigned-vector-prefix? c) (begin (read-char prt)
                                         (read-unsigned-vector prt)))
     ((vector-start? c) (begin (read-char prt) (read-vector-elements prt)))
     ((char-escape-prefix? c) (begin (read-char prt) (read-escaped-char prt))))))

(define (read-prefixed-number prt)
  (define prefix (read-char prt))
  (string->number (string-append "#"
                                 (list->string (list prefix))
                                 ;; don't wanna use read-symbol... here but works for now
                                 (symbol->string (read-symbol-from-port prt)))))

(define (read-unsigned-vector prt)
  (define size
    (let ((c (read-char prt)))
      (unless (char=? #\8 c)
        (error "unsupported unsigned size vector size" c))
      c))
  (define paren (read-char prt))
  (unless (char=? #\()
    (error "invalid paren char for bytevector" paren))
  (let ((els (yl-read-list-elements prt)))
    (apply bytevector
           (map (lambda (el)
                  (if (and (number? el)
                           (in-byte-range? el))
                      el
                      (error "unsupported bytevector element" el)))
                els))))

(define (read-vector-elements prt)
  (list->vector (yl-read-list-elements prt)))

(define (make-in-range-pred min max)
  (lambda (n)
    (and (>= n min)
         (<= n max))))

(define in-byte-range? (make-in-range-pred 0 255))

(define (read-escaped-char prt)
  (with-peeked-char c prt
    (if (symbol-terminal-char? c)
        (read-char prt)
        (let* ((rest (symbol->string (read-symbol-from-port prt)))
               (chr (assoc rest
                           '(("alarm"     . #\alarm)
                             ("backspace" . #\backspace)
                             ("delete"    . #\delete)
                             ("escape"    . #\escape)
                             ("newline"   . #\newline)
                             ("null"      . #\null)
                             ("return"    . #\return)
                             ("space"     . #\space)
                             ("tab"       . #\tab)))))
          (if (= 1 (string-length rest))
              (car (string->list rest)) ; string->char
              (if chr
                  (cdr chr)
                  (error "unsupported char escape sequence" rest)))))))

(define (make-char-=?-fn a)
  (lambda (b) (char=? a b)))

(define char-starts-list? (make-char-=?-fn #\())
(define char-ends-list? (make-char-=?-fn #\)))
(define char-starts-string? (make-char-=?-fn #\"))
(define quote-char? (make-char-=?-fn #\'))
(define hash-char? (make-char-=?-fn #\#))

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

(define symbol-terminal-chars (string->list "\"'()[]{},`"))
(define (symbol-terminal-char? c) (member c symbol-terminal-chars))

(define (char-ends-symbol? c)
  ;; incomplete?
  (or (char-whitespace? c)
      (member c symbol-terminal-chars)))

(define number-terminal-chars symbol-terminal-chars)

(define (char-ends-number? c)
  ;; incomplete
  (or (char-whitespace? c)
      (member c number-terminal-chars)))

(define (yl-read-through-whitespace prt)
  (read-from-port-while char-whitespace? prt))

(define (char->string c)
  (vector->string (vector c)))

(let ((prt
       ;(open-input-string "   (  aff \n \tbaff  mm a fo) ooo")
       (open-input-string " \"hello\\n \\tw\\\"o\\\"rld\" #u8(1 2 14 42)  -123 +321 -QED-  (  aff \n \tbaff  mm a fo) ooo")
       ))
  (newline)
  (display (yl-read prt))
  (newline)
  (display (yl-read prt))
  (newline)
  (display (yl-read prt))
  (newline)
  (display (eof-object? (peek-char prt)))
  (close-input-port prt))

;(define test-file (open-input-file "test.scm"))
;(close-port test-file)
