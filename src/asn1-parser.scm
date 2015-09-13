(define-module (asn1-parser)
  #:use-module (system base lalr)
  #:use-module (asn1-tokenize)
  #:export (read-asn1 asn1-parser))

(define (syntax-error message . args)
  (apply throw 'SyntaxError message args))

(define (read-asn1 port)
  (asn1-parser (make-asn1-tokenizer port) syntax-error))

(define *eof-object*
  (call-with-input-string "" read-char))


(define asn1-parser
  (lalr-parser
   ; Terminal symbols
   (dot dotdot dotdotdot
    comma
    semicolon
    lbrace rbrace
    lbracket rbrack
    lparen rparen
    langle rangle
    minus bar ::= backslash

    ABSENT ENCODED INTEGER RELATIVE-OID
    ABSTRACT-SYNTAX END INTERSECTION SEQUENCE
    ALL ENUMERATED SET APPLICATION EXCEPT MAX SIZE
    AUTOMATIC EXPLICIT MIN STRING OF
    BEGIN EXPORTS MINUS-INFINITY SYNTAX
    BIT EXTENSIBILITY NULL EXTERNAL TAGS
    BOOLEAN FALSE OBJECT BY FROM TRUE
    CHARACTER OCTET TYPE-IDENTIFIER
    CHOICE UNION CLASS OPTIONAL UNIQUE
    COMPONENT IA5String PATTERN UNIVERSAL
    COMPONENTS IDENTIFIER PDV
    CONSTRAINED IMPLICIT PLUS-INFINITY UTCTime
    CONTAINING IMPLIED PRESENT EMBEDDED INSTANCE REAL WITH
    DEFAULT IMPORTS DEFINITIONS INCLUDES PRIVATE
    ;; Additional keywords
    MACRO TYPE VALUE NOTATION
    MODULE-IDENTITY OBJECT-TYPE NOTIFICATION-TYPE TRAP-TYPE
    TEXTUAL-CONVENTION MODULE-COMPLIANCE OBJECT-GROUP
    NOTIFICATION-GROUP OBJECT-IDENTITY MODULE AGENT-CAPABILITIES
    WRITE-SYNTAX

    :id :number :string )

  (%root
    (%module-definition) : $1)

  (%module-definition
    (:id DEFINITIONS %tag-default %extension-default ::= BEGIN %module-body END) : `(module ,$1 ,$7))

  (%tag-default
    () : (list))

  (%extension-default
    () : (list))

  (%module-body
    (%exports %imports %assignment-list) : `(,$1 ,$2 ,$3)
    () : (list))

  (%exports
    (EXPORTS ALL semicolon) : `(export :all)
    (EXPORTS %symbol* semicolon) : `(export ,@$2)
    () : (list))

  (%imports
    (IMPORTS %symbols-from-modules semicolon) : `(import ,@$2)
    () : (list))

  (%symbols-from-modules
    (%symbols-from-module) : `(,$1)
    (%symbols-from-modules %symbols-from-module) : `(,@$1 ,$2))

  (%symbols-from-module
    (%symbol+ FROM :id) : `(,$1 :from ,$3))

  (%assignment-list
    (%assignment-list %assignment) : `(,@$1 ,$2)
    (%assignment) : `(,$1))

  (%assignment
    (%macro-definition) : $1
    (%type-assignment) : $1
    (%value-assignment) : $1)

  ;; ASN.1 Macro
  (%macro-definition
    (%macro-name MACRO ::= BEGIN %general-list END) : `(:macro ,$1))

  ;; ASN.1 Value Assignment
  (%value-assignment
    (:id %type ::= %value) : `(:value-assignment ,$2 (,$1 ,$4))
    (:id %macro-name %macro-arguments+ ::= %object-identifier-value) : `(:define ,$2 (,$1 ,$3) ,$5)
    (:id %macro-name %macro-arguments+ ::= :number) : `(:define ,$2 (,$1 ,$3) ,$5))

  (%macro-arguments+
    (%macro-arguments) : `(,$1)
    (%macro-arguments+ %macro-arguments) : `(,@$1 ,$2))

  (%macro-arguments
    (:id :id) : `(,$1 ,$2)
    (:id :string) : `(,$1 ,$2)
    (SYNTAX %type) : `(,$1 ,$2)
    (WRITE-SYNTAX %type) : `(,$1 ,$2)
    (OBJECT :id) : `(,$1 ,$2)
    (MODULE :id %macro-arguments+) : `(:module ,$2 ,$3)
    (MODULE %macro-arguments+) : `(:module nil ,$2)
    (:id lbrace lbrace %symbol* rbrace rbrace) : `(,$1 ,$4)
    (:id lbrace :string rbrace) : `(,$1 ,$3)
    (:id lbrace :number rbrace) : `(,$1 ,$3)
    (:id lbrace %implied-symbol+ rbrace) : `(,$1 ,$3))

  ;; ASN.1 Type Assignment
  ;;
  (%type-assignment
    (:id ::= %type) : `(:type-assignment ,$1 ,$3))

  (%type
    (%builtin-type) : $1
    (%tagged-type) : $1
    (:id) : $1
    (:id lparen SIZE lparen %numbers+ rparen rparen) : `(:general-string ,$1 ,$5)
    (:id lparen %numbers+ rparen) : `(:general-integer ,$1 ,$3)
    (:id lbrace %named-number+ rbrace) : `(:general-integer ,$1 ,$3)
    (%named-type) : `(:named-type ,$1))

  (%named-type (:id %type) : `(,$1 ,$2))

  (%builtin-type
    (%object-identifier-type) : $1
    (%choice-type) : $1
    (%string-type) : $1
    (%integer-type) : $1
    (%sequence-of-type) : $1
    (%sequence-type) : $1
    (%textual-convention-type) : $1
    (NULL) : `(:null))

  (%object-identifier-type
    (OBJECT IDENTIFIER) : `(:object-identifier))

  (%choice-type
    (CHOICE lbrace %alternative-type-lists rbrace) : `(:choice ,$3))

  (%alternative-type-lists
    (%root-alternative-type-list) : $1)

  (%root-alternative-type-list
    (%alternative-type-list) : $1)

  (%alternative-type-list
    (%named-type) : `(,$1)
    (%alternative-type-list comma %named-type) : `(,@$1 ,$3))

  (%string-type
    (OCTET STRING %string-options) : `(:octet-string ,$3))

  (%string-options
    (lparen SIZE lparen %numbers+ rparen rparen) : `(:size ,$4)
    (lparen %numbers+ rparen) : `(:size ,$2)
    () : (list))

  (%numbers+
    (%numbers+ bar %splited-numbers) : `(,@$1 ,$3)
    (%splited-numbers) : `(,$1))

  (%integer-type
    (%integer-type-name lparen %numbers+ rparen) : `(,$1 ,$3)
    (%integer-type-name lbrace %named-number+ rbrace) : `(,$1 ,$3)
    (%integer-type-name) : $1)

  (%integer-type-name
     (INTEGER) : :integer)

  (%splited-numbers
    (:number) : $1
    (:number dotdot :number) : `(,$1 ,$3))

  (%named-number+
    (%named-number) : `(,$1)
    (%named-number+ comma %named-number) : `(,@$1 ,$3))

  (%named-number
    (:id lparen :number rparen) : `(,$1 ,$3))

  (%tagged-type
    (%tag IMPLICIT %builtin-type) : `(:implicit ,$1 ,$3)
    (%tag EXPLICIT %builtin-type) : `(:explicit ,$1 ,$3)
    (%tag %builtin-type) : `(:tag ,$1 ,$2))

  (%tag
    (lbracket %class :number lbracket) : `(,$2 ,$3))

  (%class
    (UNIVERSAL) : :universal
    (APPLICATION) : :application
    (PRIVATE) : :private
    () : (list))

  (%value
    (%object-identifier-value) : $1
    (:string) : $1
    (:number) : $1)

  (%object-identifier-value
    (lbrace %obj-id-component+ rbrace) : `(,@$2))

  (%obj-id-component+
    (%obj-id-component+ %obj-id-component) : `(,@$1 ,$2)
    (%obj-id-component) : `(,$1))

  (%obj-id-component
    (%name-and-number-form) : $1
    (:id) : $1
    (:number) : $1)

  (%name-and-number-form
    (:id lparen :number rparen) : `(,$1 ,$3))

  (%sequence-of-type
    (SEQUENCE OF %type) : `(:sequence-of ,$3))

  (%sequence-type
    (SEQUENCE lbrace rbrace) : `(:sequence)
    (SEQUENCE lbrace %component-type-lists rbrace) : `(:sequence ,@$3))

  (%component-type-lists
    (%root-component-type-list) : $1)

  (%root-component-type-list
    (%component-type-list) : $1)

  (%component-type-list
    (%component-type) : `(,$1)
    (%component-type-list comma %component-type) : `(,@$1 ,$3))

  (%component-type
    (%named-type OPTIONAL) : `(,$1 :optional)
    (%named-type DEFAULT %value) : `(,$1 :default ,$3)
    (%named-type) : $1
    (COMPONENTS OF %type) : `(:components-of ,$3))

  (%textual-convention-type
    (TEXTUAL-CONVENTION %tc-args SYNTAX %type) : `(:textual-convention ,$2 (:syntax ,$4)))

  (%tc-args
    (%tc-arg) : `(,$1)
    (%tc-args %tc-arg) : `(,@$1 ,$2))

  (%tc-arg
    (:id :id) : `(,$1 ,$2)
    (:id :string) : `(,$1 ,$2))

  ;; Symbol+ and Symbol*
  (%symbol+
    (%symbol) : `(,$1)
    (%symbol+ comma %symbol) : `(,@$1 ,$3))

  (%implied-symbol+
    (%implied-symbol) : `(,$1)
    (%implied-symbol+ comma %implied-symbol) : `(,@$1 ,$3))

  (%symbol*
    (%symbol) : `(,$1)
    (%symbol* comma %symbol) : `(,@$1 ,$3)
    () : (list))

  (%symbol
    (%macro-name) : $1
    (:id) : $1)

  (%implied-symbol
    (:id) : $1
    (IMPLIED :id) : `(:implied ,$2))

  (%macro-name
    (MODULE-IDENTITY) : $1
    (OBJECT-TYPE) : $1
    (NOTIFICATION-TYPE) : $1
    (TEXTUAL-CONVENTION) : $1
    (MODULE-COMPLIANCE) : $1
    (OBJECT-GROUP) : $1
    (NOTIFICATION-GROUP) : $1
    (OBJECT-IDENTITY) : $1
    (AGENT-CAPABILITIES) : $1
    (TRAP-TYPE) : $1)

  ;;; Last Rules
  (%general-list
    (%general) : `(,$1)
    (%general-list comma %general) : `(,@$1 ,$2)
    (%general-list %general) : `(,@$1 ,$2))

  (%general
    (:number) : $1
    (:id) : $1
    (:string) : $1
    (::=) : $1
    (lparen) : $1
    (rparen) : $1
    (backslash) : $1
    (lbrace) : $1
    (rbrace) : $1
    (langle) : $1
    (rangle) : $1
    (TYPE) : $1
    (VALUE) : $1
    (NOTATION) : $1
    (SEQUENCE) : $1
    (OBJECT) : $1
    (IDENTIFIER) : $1
    (INTEGER) : $1
    (IA5String) : $1)))


