#lang plai-typed

(require plai-typed/s-exp-match)
(print-only-errors true)

;; Esha Joshi
;; ejoshi@calpoly.edu
;; CPE 430 - 01, Assignment3
;; 05.04.2015

;; Rudimentary Interpreter: Parser and interpreter for the Excr2 language

;; Representing the language
(define-type EXCR2
  [numC (n : number)]
  [idC (s : symbol)]
  [bool (b : boolean)]
  [if (if : EXCR2) (then : EXCR2) (els : EXCR2)]
  [binOp (operator : symbol) (l : EXCR2) (r : EXCR2)]
  ;; Function call that takes a function definition (another EXCR2) and a list of EXCR2 as its arguments
  [funcCall (funcDef : EXCR2) 
            (arguments : (listof EXCR2))]
  ;; Definition of a function defined by its arguments and body
  [fundefC (args : (listof symbol))
           (body : EXCR2)])

;; Representation of a Binding
;; Binds a name to a Value
(define-type Binding
  [bind (name : symbol) (val : Value)])

;; Representation of a Value
(define-type Value
  [numV (n : number)]
  [strV (s : string)]
  [boolV (b : boolean)]
  [cloV (param : (listof symbol))
        (body : EXCR2)
        (env : Env)])

;; Environment represented as a listof Bindings
(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

;#############################

;; Function check-symbol : symbol -> boolean
;; Determines whether passed in symbol is a binary or reserved operator
(define (check-symbol [s : symbol]) : boolean
  (cond
    [(or (eq? s '+)
         (eq? s '-)
         (eq? s '*)
         (eq? s '/)
         (eq? s 'if)
         (eq? s 'with)
         (eq? s 'plus)) #f]
    [else #t]))

(test (check-symbol '+) #f)
(test (check-symbol '-) #f)
(test (check-symbol '*) #f)
(test (check-symbol '/) #f)
(test (check-symbol 'plus) #f)
(test (check-symbol 'func) #t)
(test (check-symbol 'esha) #t)

;#############################

;; Function serialize : Value -> string
(define (serialize [v : Value]) : string
  (cond
    [(numV? v) (to-string (numV-n v))]
    [(cloV? v) "#<procedure>"]
    [(and (boolV? v) (eq? (boolV-b v) #t)) "true"]
    [(and (boolV? v) (eq? (boolV-b v) #f)) "false"]
    [else "serialize not used"]))

(test (serialize (numV 15)) "15")
(test (serialize (cloV (list 'x) (numC 5) empty)) "#<procedure>")
(test (serialize (boolV #t)) "true")
(test (serialize (boolV #f)) "false")
(test (serialize (strV "abc")) "serialize not used")

;#############################

;; Function get-with-param : (listof s-expression) -> (listof symbol)
;; Parses through the 'with' s-expression and adds all the param (symbols) to a list
(define (get-with-param [s : (listof s-expression)]) : (listof symbol)
  (cond [(empty? (rest s)) empty]
        [else (cons (s-exp->symbol (first (s-exp->list (first s)))) (get-with-param (rest s)))]))

;#############################

;; Function get-with-values : (listof s-expression) -> (listof EXCR2)
;; Parses through the 'with' s-expression and adds all the values (EXCR2) to a list
(define (get-with-values [s : (listof s-expression)]) : (listof EXCR2)
  (cond
    [(empty? (rest s)) empty]
    [else (cons (parse (third (s-exp->list (first s)))) (get-with-values (rest s)))]))

;#############################

;; Functions get-with-body : (listof s-expression) -> EXCR2
;; Parses through the 'with' s-expression and returns the body
(define (get-with-body [s : (listof s-expression)]) : EXCR2
  (let ([reversed (reverse s)])
    (parse (first reversed))))

;#############################

;; Function check-param-name : (listof symbol) -> boolean
;; Parses through the list of parameter names and determines if there are any duplicates
(define (check-param-name [l : (listof symbol)]) : boolean
   (cond
     [(empty? l) #t]
     [(eq? (member (first l) (rest l)) #t) (error 'parse "duplicate param names")]
     [else (check-param-name (rest l))]))

;; Test Cases for check-param-name
(test/exn (check-param-name (list 'a 'b 'b 'd 'e)) "parse: duplicate param names")
(test (check-param-name (list 'one 'two 'three)) #t)

;#############################

;; Function convert-to-symbols : (listof s-expression) -> (listof symbol)
;; Converts the list of s-expressions to the list of symbols
(define (convert-to-symbols  [s : (listof s-expression)]) : (listof symbol)
  (cond
    [(empty? s) empty]
    [else (cons (s-exp->symbol (first s)) (convert-to-symbols (rest s)))]))

;;; Test Cases for convert-to-symbols
(test (convert-to-symbols (list `a `b `c)) (list 'a 'b 'c))
(test (convert-to-symbols empty) empty)

;#############################

;; Function check-binOp : symbol -> boolean
;; Checks a binOp
(define (check-binop [s : symbol]) : boolean
  (cond
    [(or (eq? s '+)
         (eq? s '-)
         (eq? s '*)
         (eq? s '/)
         (eq? s '<=)
         (eq? s 'eq?)) #t]
    [else #f]))

(test (check-binop '+) #t)
(test (check-binop '-) #t)
(test (check-binop '*) #t)
(test (check-binop '/) #t)
(test (check-binop '&) #f)
(test (check-binop '$) #f)

;#############################

;; Function parse : s-expression -> EXCR2
;; Parses s-expressions to yield EXCR2
(define (parse [s : s-expression]) : EXCR2
  (cond 
    ;; Parse numbers
    [(s-exp-number? s) (numC (s-exp->number s))]
    ;; Parse boolean - false
    [(s-exp-match? `false s) (bool #f)]
    ;; Parse boolean - true
    [(s-exp-match? `true s) (bool #t)]
    ;; Parse symbols
    [(s-exp-symbol? s) 
     (cond 
       [(check-symbol (s-exp->symbol s)) (idC (s-exp->symbol s))]
       [else (error 'parse "invalid input")])]
    ;; Parse lists
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         ;; Parse if
         [(s-exp-match? '{if ANY ANY ANY} s) 
          (if (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         ;; Parse fundefC
         [(s-exp-match? '{fn ANY ANY} s)
          (cond
            [(check-param-name (convert-to-symbols (s-exp->list (second sl))))
             (fundefC (map s-exp->symbol (s-exp->list (second sl)))
                      (parse (third sl)))])]
         ;; Parse with syntax
         [(s-exp-match? '{with {ANY = ANY} ... ANY} s)
          (cond
            [(check-param-name (get-with-param (rest sl))) 
             (funcCall (fundefC (get-with-param (rest sl)) (get-with-body (rest sl))) (get-with-values (rest sl)))])]
         ;; Parse funcCall
         [(or (s-exp-match? '{(ANY) ANY ...} s) (s-exp-match? '{(ANY ANY ...)} s))
          (funcCall (parse (first sl)) (map parse (rest sl)))]
         ;; Parse binOp
         [(and (s-exp-match? '{SYMBOL ANY ANY} s) (check-binop (s-exp->symbol (first sl))))
          (cond
            [(eq? 3 (length sl)) (binOp (s-exp->symbol (first sl)) (parse (second sl)) (parse (third sl)))])]
         ;; Error checking for symbols
         [(and (s-exp-symbol? (first sl)) (not (check-symbol (s-exp->symbol (first sl))))) (error 'parse (string-append "invalid input" (to-string (first sl))))]
         ;; Parse funcCall
         [(s-exp-match? '{ANY ANY ...} s)
          (funcCall (parse (first sl)) (map parse (rest sl)))]))]
    [else (error 'parse "invalid input")]))

;; Test Cases for get-with-param
(test (get-with-param (list `{z = (+ 9 14)} `{y = 98} `{+ z y})) (list 'z 'y))
(test (get-with-param (list `{a = (- 34 55)} `{+ a b})) (list 'a))

;; Test Cases for get-with-values
(test (get-with-values (list `{z = (+ 9 14)} `{y = 98} `{+ z y})) (list (binOp '+ (numC 9) (numC 14)) (numC 98)))
(test (get-with-values (list `{a = (- 34 55)} `{+ a b})) (list (binOp '- (numC 34) (numC 55))))

;; Test Cases for get-with-body
(test (get-with-body (list `{z = (+ 9 14)} `{y = 98} `{+ z y})) (binOp '+ (idC 'z) (idC 'y)))
(test (get-with-body (list `{a = (- 34 55)} `{+ a b})) (binOp '+ (idC 'a) (idC 'b)))

;; Test Cases for parse
(test (parse '4) (numC 4))
(test (parse `true) (bool #t))
(test (parse `false) (bool #f))
(test (parse '{if x x {- x 1}}) (if (idC 'x) (idC 'x) (binOp '- (idC 'x) (numC 1))))
(test (parse '(+ 1 2)) (binOp '+ (numC 1) (numC 2)))
(test (parse '(- 5 3)) (binOp '- (numC 5) (numC 3)))
(test (parse '(fn {x} (+ x x))) (fundefC (list 'x) (binOp '+ (idC 'x) (idC 'x))))
(test (parse '(fn {x y} (+ x y))) (fundefC (list 'x 'y) (binOp '+ (idC 'x) (idC 'y))))
(test (parse '(fn () (- (+ 3 10) (* 2 3)))) 
      (fundefC empty (binOp '- (binOp '+ (numC 3) (numC 10)) (binOp '* (numC 2) (numC 3)))))
(test (parse '{with {z = {+ 9 14}}
                    {y = 98}
                    {+ z y}})
      (funcCall (fundefC (list 'z 'y) (binOp '+ (idC 'z) (idC 'y))) (list (binOp '+ (numC 9) (numC 14)) (numC 98))))
(test (parse '(g 15)) (funcCall (idC 'g) (list (numC 15))))
(test (parse `((g) 15)) (funcCall (funcCall (idC 'g) empty) (list (numC 15))))
(test (parse `((fn () 9))) (funcCall (fundefC empty (numC 9)) empty))
#;(test (parse `((fn (seven) (seven)) ((fn (minus) (fn () (minus (+ 3 10) (* 2 3)))) (fn (x y) (+ x (* -1 y)))))) 
      (funcCall
       (fundefC (list 'seven) (funcCall (idC 'seven) empty))
       (list
        (funcCall
         (fundefC (list 'minus) (fundefC empty (funcCall (idC 'minus) (list (binOp '+ (numC 3) (numC 10)) (funcCall (idC '*2) (list (numC 3)))))))
         (list (fundefC (list 'x 'y) (binOp '+ (idC 'x) (binOp '* (numC -1) (idC 'y)))))))))

;(test/exn (parse '(4 5 5)) "s-exp->symbol: not a symbol: 4")
(test/exn (parse '"abc") "parse: invalid input")
(test/exn (parse '(/ 3 4 5)) "parse: invalid input")
(test/exn (parse '(/ * 3)) "parse: invalid input")
(test/exn (parse `if) "parse: invalid input")
(test/exn (parse '{plus}) "parse: invalid input")
(test/exn (parse '"cookie") "parse: invalid input")
(test/exn (parse '(fn {+} (+ + +))) "parse: invalid input")
(test/exn (parse '(fn + () 13)) "parse: invalid input")
(test/exn (parse '(- 1 2 3)) "parse: invalid input")
(test/exn (parse `(fn {x x} (numC 3))) "parse: duplicate param names")
(test/exn (parse `(with {z = (fn empty 3)} 
                        {z = 9} 
                        {z})) "parse: duplicate param names")

;#############################

;; Function env-lookup : symbol Env -> Value
;; Looks up Binding in an environment
(define (env-lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'env-lookup (string-append "name not found: " (to-string for)))]
    [else (cond
            [(eq? for (bind-name (first env))) (bind-val (first env))]
            [else (env-lookup for (rest env))])]))

(test/exn (env-lookup 'noBinding (list (bind 'bindbaby (numV 123)))) "name not found")
(test (env-lookup 'z (list (bind 'xxx (strV "wheee!")) (bind 'z (strV "134")))) (strV "134"))
(test (env-lookup 'esha (list (bind 'binding (numV 69)) (bind 'esha (strV "someBindings")))) (strV "someBindings"))

;#############################

;; Function num+ : Value Value -> Value
;; Checks both arguments are numbers before performing the addition
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
    [else (error 'num+ "invalid input")]))

(test (num+ (numV 12) (numV 2)) (numV 14))
(test/exn (num+ (strV "123") (numV 5)) "invalid input")

;; Functions num- : Value Value -> Value
;; Checks both arguments are numbers before performing the subtraction
(define (num- [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (- (numV-n l) (numV-n r)))]
    [else (error 'num- "invalid input")]))

(test (num- (numV 12) (numV 2)) (numV 10))
(test/exn (num- (strV "123") (numV 5)) "invalid input")

;; Functions num* : Value Value -> Value
;; Checks both arguments are numbers before performing the multiplication
(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
    [else (error 'num- "invalid input")]))

(test (num* (numV 12) (numV 2)) (numV 24))
(test/exn (num* (strV "123") (numV 5)) "invalid input")

;; Functions num/ : Value Value -> Value
;; Checks both arguments are numbers before performing the division
(define (num/ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (/ (numV-n l) (numV-n r)))]
    [else (error 'num/ "invalid input")]))

(test (num/ (numV 12) (numV 2)) (numV 6))
(test/exn (num/ (strV "123") (numV 5)) "invalid input")

;; Functions num<= : Value Value -> boolean
;; Validate both arguments are numbers and returns true if the 1st arg is <= to the 2nd arg
(define (num<= [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (boolV (<= (numV-n l) (numV-n r)))]
    [else (error 'num<= "invalid input")]))
  
(test (num<= (numV 2) (numV 3)) (boolV #t))
(test/exn (num<= (strV "123") (numV 5)) "invalid input")

;; Functions val-eq : Value Value -> boolean
;; Consumes two values and returns true if given two booleans with the same value,
;; or two numbers with the same value
(define (val-eq [l : Value] [r : Value]) : Value
  (cond
    [(and (and (boolV? l) (boolV? r)) (eq? (boolV-b l) (boolV-b r))) (boolV #t)]
    [(and (and (numV? l) (numV? r)) (eq? (numV-n l) (numV-n r))) (boolV #t)]
    [else (boolV #f)]))
  
(test (val-eq (boolV #t) (boolV #f)) (boolV #f))
(test (val-eq (boolV #f) (boolV #f)) (boolV #t))
(test (val-eq (numV 5) (numV 5)) (boolV #t))
(test (val-eq (boolV #t) (numV 5)) (boolV #f))

;#############################

;; Function interp-helper : (listof symbol) (listof EXCR2) Env -> Env
;; Adds bindings to an environment
(define (interp-helper [fdArgs : (listof symbol)] [args : (listof EXCR2)] [env : Env]) : Env
  (cond [(empty? args) empty]
        [else 
         (extend-env (bind (first fdArgs) (interp (first args) env)) 
                          (interp-helper (rest fdArgs) (rest args) env))]))

;#############################

;; Function interp : EXCR2 Env -> Value
(define (interp [e : EXCR2] [env : Env]) : Value
  (type-case EXCR2 e
    [numC (n) (numV n)]
    [idC (s) (env-lookup s env)]
    [bool (b) (boolV b)]
    [if (testNum then els)
           (cond
             [(boolV-b (val-eq (interp testNum env) (boolV #t))) (interp then env)]
             [else (interp els env)])]
    [binOp (operator l r)
           (cond 
             [(eq? operator '+) (num+ (interp l env) (interp r env))]
             [(eq? operator '-) (num- (interp l env) (interp r env))]
             [(eq? operator '*) (num* (interp l env) (interp r env))]
             [(eq? operator '/) (num/ (interp l env) (interp r env))]
             [(eq? operator '<=) (num<= (interp l env) (interp r env))]
             [(eq? operator 'eq?) (val-eq (interp l env) (interp r env))])]
    [funcCall (funcDef args) 
              (let ([defn (interp funcDef env)])
                (type-case Value defn
                  [cloV (param body cloEnv) (interp body (append (interp-helper param args env) cloEnv))]
                  [else (error 'parse "wrong arity")]))]
    [fundefC (argument body)
             (cloV argument body env)]))

;; Library of Bindings aka Environment
(define MY-BINDINGS
  (list (bind 'binding (numV 10))
        (bind 'esha (boolV #t))
        (bind 'someBinding (numV 15))))

;; Test Cases for interp-helper
(test (interp-helper (list 'x 'y 'z) (list (numC 1) (numC 2) (numC 3)) empty)  
      (list (bind 'x (numV 1)) (bind 'y (numV 2)) (bind 'z (numV 3))))

;; Test Cases for interp
(test (interp (numC 10) (list (bind 'binding (numV 10)))) (numV 10))
(test (interp (idC 'esha) (list (bind 'xy (numV 10)) (bind 'esha (boolV #t)))) (boolV #t))
(test (interp (bool #f) (list (bind 'someBinding (strV "abc")))) (boolV #f))
(test (interp (if (bool #t) (binOp '* (numC 5) (numC 6)) (binOp '/ (numC 10) (numC 2))) MY-BINDINGS) (numV 30))
(test (interp (if (bool #f) (binOp '* (numC 5) (numC 6)) (binOp '/ (numC 10) (numC 2))) MY-BINDINGS) (numV 5))
(test (interp (binOp '+ (numC 5) (numC 6)) MY-BINDINGS) (numV 11))
(test (interp (binOp '- (numC 5) (numC 6)) MY-BINDINGS) (numV -1))
(test (interp (binOp '<= (numC 5) (numC 6)) MY-BINDINGS) (boolV #t))
(test (interp (binOp 'eq? (numC 5) (numC 6)) MY-BINDINGS) (boolV #f))
(test (interp (funcCall (fundefC (list 'x 'y) (binOp '+ (idC 'x) (idC 'y))) (list (numC 5) (numC 6))) empty) (numV 11))
(test/exn (interp (funcCall (binOp '+ (numC 5) (numC 6)) (list (numC 5) (numC 6))) MY-BINDINGS) "parse: wrong arity")

;#############################

;; Function top-eval : s-expression -> string
(define (top-eval [s : s-expression]) : string
  (serialize (interp (parse s) mt-env)))

(test (top-eval '(+ 4 1)) "5")
(test (top-eval '(- 7 3)) "4")
(test (top-eval '(* 5 8)) "40")
(test (top-eval '(/ 60 12)) "5")
(test (top-eval `((fn (minus) (minus 8 5)) (fn (a b) (+ a (* -1 b))))) "3")

;(test (top-eval `(if true 3 4)) "3")