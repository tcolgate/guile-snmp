;; Copyright (C) 2009 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (asn1-tokenize)
  #:use-module (ice-9 rdelim)
  #:use-module ((srfi srfi-1) #:select (unfold-right))
  #:export (next-token make-tokenizer tokenize))

(define (syntax-error message . args)
  (apply throw 'SyntaxError message args))

; keyworkds
(define *asn1-tokens*           
  '(("ABSENT" . ABSENT)
    ("ENCODED" . ENCODED)
    ("INTEGER" . INTEGER)
    ("RELATIVE-OID" . RELATIVE-OID)
    ("ABSTRACT-SYNTAX" . ABSTRACT-SYNTAX)
    ("END" . END)
    ("INTERSECTION" . INTERSECTION)
    ("SEQUENCE" . SEQUENCE)
    ("ALL" . ALL)
    ("ENUMERATED" . ENUMERATED)
    ("SET" . SET)
    ("APPLICATION" . APPLICATION)
    ("EXCEPT" . EXCEPT)
    ("MAX" . MAX)
    ("SIZE" . SIZE)
    ("AUTOMATIC" . AUTOMATIC)
    ("EXPLICIT" . EXPLICIT)
    ("MIN" . MIN)
    ("STRING" . STRING)
    ("BEGIN" . BEGIN)
    ("EXPORTS" . EXPORTS)
    ("MINUS-INFINITY" . MINUS-INFINITY)
    ("SYNTAX" . SYNTAX)
    ("BIT" . BIT)
    ("EXTENSIBILITY" . EXTENSIBILITY)
    ("NULL" . NULL)
    ("EXTERNAL" . EXTERNAL)
    ("TAGS" . TAGS)
    ("BOOLEAN" . BOOLEAN)
    ("FALSE" . FALSE)
    ("OBJECT" . OBJECT)
    ("BY" . BY)
    ("FROM" . FROM)
    ("TRUE" . TRUE)
    ("CHARACTER" . CHARACTER)
    ("OCTET" . OCTET)
    ("TYPE-IDENTIFIER" . TYPE-IDENTIFIER)
    ("CHOICE" . CHOICE)
    ("OF" . OF)
    ("UNION" . UNION)
    ("CLASS" . CLASS)
    ("OPTIONAL" . OPTIONAL)
    ("UNIQUE" . UNIQUE)
    ("COMPONENT" . COMPONENT)
    ("PATTERN" . PATTERN)
    ("UNIVERSAL" . UNIVERSAL)
    ("COMPONENTS" . COMPONENTS)
    ("IDENTIFIER" . IDENTIFIER)
    ("PDV" . PDV)
    ("CONSTRAINED" . CONSTRAINED)
    ("IMPLICIT" . IMPLICIT)
    ("PLUS-INFINITY" . PLUS-INFINITY)
    ("CONTAINING" . CONTAINING)
    ("IMPLIED" . IMPLIED)
    ("PRESENT" . PRESENT)
    ("DEFAULT" . DEFAULT)
    ("IMPORTS" . IMPORTS)
    ("DEFINITIONS" . DEFINITIONS)
    ("INCLUDES" . INCLUDES)
    ("PRIVATE" . PRIVATE)
    ("EMBEDDED" . EMBEDDED)
    ("INSTANCE" . INSTANCE)
    ("REAL" . REAL)
    ("WITH" . WITH)
    ("MACRO" . MACRO) 
    ("TYPE" . TYPE) 
    ("VALUE" . VALUE) 
    ("NOTATION" . NOTATION) 
    ("MODULE-IDENTITY" . MODULE-IDENTITY) 
    ("OBJECT-TYPE" . OBJECT-TYPE) 
    ("NOTIFICATION-TYPE" . NOTIFICATION-TYPE) 
    ("TRAP-TYPE" . TRAP-TYPE) 
    ("TEXTUAL-CONVENTION" . TEXTUAL-CONVENTION) 
    ("MODULE-COMPLIANCE" . MODULE-COMPLIANCE) 
    ("OBJECT-GROUP" . OBJECT-GROUP) 
    ("NOTIFICATION-GROUP" . NOTIFICATION-GROUP) 
    ("OBJECT-IDENTITY" . OBJECT-IDENTITY) 
    ("MODULE" . MODULE) 
    ("AGENT-CAPABILITIES" . AGENT-CAPABILITIES) 
    ("WRITE-SYNTAX" . WRITE-SYNTAX)))

(define *asn1-punctuation*          
  '(("." . dot)
    (".." . dotdot)
    ("..." . dotdotdot)
    ("-" . minus)
    ("--" . minusminus)
    ("|" . bar) 
    (":" . colon)
    (";" . semicolon)
    ("::=" . assign)
    ("/" . forwardslash)
    ("\\" . backslash)
    ("," . comma)
    ("{" . lbrace)
    ("}" . rbrace)
    ("[" . lbracket)
    ("]" . rbracket)
    ("(" . lparen)
    (")" . rparen)
    ("<" . langle)
    (">" . rangle)))

;; taken from SSAX, sorta
(define (read-until delims port)
  (if (eof-object? (peek-char port))
      (syntax-error "EOF while reading a token")
      (let ((token (read-delimited delims port 'peek)))
        (if (eof-object? (peek-char port))
            (syntax-error "EOF while reading a token")
            token))))

; Single minux requires a lookahead
(define (reader- port)
  (let ((next-char (peek-char port)))
    (case next-char
      ((#\-) (reader-- port))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (unread-char next-char port)
       (let ((possible-number (read-numeric port)))
         `(:number  . ,(- possible-number))))
      (else (unread-char next-char port) '(minus . #f)))))

; Double minus is definitiely a comment
(define (reader-- port)
  `(:comment .  ,(read-until (string #\newline) port)))

(define (char-hex? c)
  (and (not (eof-object? c))
       (or (char-numeric? c)
           (memv c '(#\a #\b #\c #\d #\e #\f))
           (memv c '(#\A #\B #\C #\D #\E #\F)))))

(define (digit->number c)
  (- (char->integer c) (char->integer #\0)))

(define (hex->number c)
  (if (char-numeric? c)
      (digit->number c)
      (+ 10 (- (char->integer (char-downcase c)) (char->integer #\a)))))

(define (read-string port)
  (let ((c (read-char port)))
    (let ((terms (string c #\\)))
      (define (read-escape port)
        (let ((c (read-char port)))
          (case c
            ((#\' #\" #\\) c)
            ((#\b) #\bs)
            ((#\f) #\np)
            ((#\n) #\nl)
            ((#\r) #\cr)
            ((#\t) #\tab)
            ((#\v) #\vt)
            ((#\0)
             (let ((next (peek-char port)))
               (cond ((eof-object? next) #\nul)
                     ((char-numeric? next)
                      (syntax-error "octal escape sequences are not supported"))
                     (else #\nul))))
            ((#\x)
             (let* ((a (read-char port))
                    (b (read-char port)))
               (cond
                ((and (char-hex? a) (char-hex? b))
                 (integer->char (+ (* 16 (hex->number a)) (hex->number b))))
                (else
                 (syntax-error "bad hex character escape" a b)))))
            ((#\u)
             (syntax-error "unicode not supported"))
            (else
             c))))
      (let lp ((str (read-until terms port)))
        (let ((terminator (peek-char port)))
          (cond
           ((char=? terminator c)
            (read-char port)
            str)
           ((char=? terminator #\\)
            (read-char port)
            (let ((echar (read-escape port)))
              (lp (string-append str (string echar)
                                 (read-until terms port)))))
           ))))))


(define (read-identifier port)
  (let lp ((c (peek-char port)) (chars '()))
    (if (or (eof-object? c)
            (not (or (char-alphabetic? c)
                     (char-numeric? c)
                     (char=? c #\-)
                     (char=? c #\_))))
        (let ((word (list->string (reverse chars))))
          (cond ((assoc-ref *asn1-tokens* word)
                 `(,(string->symbol word) . #f))
                (else `(:identifier . ,(string->symbol word)))))
        (begin (read-char port)
               (lp (peek-char port) (cons c chars))))))

(define (read-numeric port)
  (let* ((c0 (if (char=? (peek-char port) #\.)
                 #\0
                 (read-char port)))
         (c1 (peek-char port)))
    (cond
     ((eof-object? c1) (digit->number c0))
     ((and (char=? c0 #\0) (char=? c1 #\x))
      (read-char port)
      (let ((c (peek-char port)))
        (if (not (char-hex? c))
            (syntax-error "bad digit reading hexadecimal number" c))
        (let lp ((c c) (acc 0))
          (cond ((char-hex? c)
                 (read-char port)
                 (lp (peek-char port)
                     (+ (* 16 acc) (hex->number c))))
                (else
                 acc)))))
     ((and (char=? c0 #\0) (char-numeric? c1))
      (let lp ((c c1) (acc 0))
        (cond ((eof-object? c) acc)
              ((char-numeric? c)
               (if (or (char=? c #\8) (char=? c #\9))
                   (syntax-error "invalid digit in octal sequence" c))
               (read-char port)
               (lp (peek-char port)
                   (+ (* 8 acc) (digit->number c))))
              (else
               acc))))
     (else
      (let lp ((c1 c1) (acc (digit->number c0)))
        (cond
         ((eof-object? c1) acc)
         ((char-numeric? c1)
          (read-char port)
          (lp (peek-char port)
              (+ (* 10 acc) (digit->number c1))))
         ((or (char=? c1 #\e) (char=? c1 #\E))
          (read-char port)
          (let ((add (let ((c (peek-char port)))
                       (cond ((eof-object? c) (syntax-error "error reading exponent: EOF"))
                             ((char=? c #\+) (read-char port) +)
                             ((char=? c #\-) (read-char port) -)
                             ((char-numeric? c) +)
                             (else (syntax-error "error reading exponent: non-digit"
                                                 c))))))
            (let lp ((c (peek-char port)) (e 0))
              (cond ((and (not (eof-object? c)) (char-numeric? c))
                     (read-char port)
                     (lp (peek-char port) (add (* 10 e) (digit->number c))))
                    (else
                     (* (if (negative? e) (* acc 1.0) acc) (expt 10 e)))))))
         ((char=? c1 #\.)
          (read-char port)
          (let lp2 ((c (peek-char port)) (dec 0.0) (n -1))
            (cond ((and (not (eof-object? c)) (char-numeric? c))
                   (read-char port)
                   (lp2 (peek-char port)
                        (+ dec (* (digit->number c) (expt 10 n)))
                        (1- n)))
                  (else
                   ;; loop back to catch an exponential part
                   (lp c (+ acc dec))))))
         (else
          acc)))))))

(define read-punctuation
  (let ((punc-tree (let lp ((nodes '())  (puncs *asn1-punctuation*))
                     (cond ((null? puncs)
                            nodes)
                           ((assv-ref nodes (string-ref (caar puncs) 0))
                            => (lambda (node-tail)
                                 (if (= (string-length (caar puncs)) 1)
                                   (set-car! node-tail (cdar puncs))
                                   (set-cdr! node-tail
                                             (lp (cdr node-tail)
                                                 `((,(substring (caar puncs) 1)
                                                     . ,(cdar puncs))))))
                                 (lp nodes (cdr puncs))))
                           (else
                             (lp (cons (list (string-ref (caar puncs) 0) #f) nodes)
                                 puncs))))))
    (lambda (port)
      (let lp ((c (peek-char port))  (tree punc-tree)  (candidate #f))
        (cond
          ((assv-ref tree c)
           => (lambda (node-tail)
                (read-char port)
                (lp (peek-char port)  (cdr node-tail)  (car node-tail))))
          (candidate
            (cons candidate #f))
          (else
            (syntax-error "bad syntax: character not allowed" )))))))


(define (next-token port div?)
  (let ((c (peek-char port))
        (props `((filename . ,(port-filename port))
                 (line . ,(port-line port))
                 (column . ,(port-column port)))))
    (let ((tok 
            (case c
              ((#\ht #\vt #\np #\space) ; whitespace
               (read-char port)
               (next-token port div?))
              ((#\newline #\cr) ; line break
               (read-char port)
               (next-token port div?))
              ((#\" #\') ; string literal
               `(:string . ,(read-string port)))
              ((#\-)
               (reader- port))
              (else
                (cond
                  ((eof-object? c)
                   '*eoi*)
                  ((char-alphabetic? c)
                   ;; reserved word or identifier
                   (read-identifier port))
                  ((char-numeric? c)
                   ;; numeric -- also accept . FIXME, requires lookahead
                   `(:number . ,(read-numeric port)))
                  (else
                    (read-punctuation port)))))))
      (if (pair? tok)
        (set-source-properties! tok props))
      
      tok)))

(define (make-tokenizer port)
  (let ((div? #f))
    (lambda ()
      (let ((tok (next-token port div?)))
        (set! div? (and (pair? tok) (eq? (car tok) 'identifier)))
        tok))))


(define (tokenize port)
  (let ((next (make-tokenizer port)))
    (let lp ((out '()))
      (let ((tok (next)))
        (if (eq? tok '*eoi*)
            (reverse! out)
            (lp (cons tok out)))))))


