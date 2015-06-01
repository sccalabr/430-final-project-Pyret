#lang plai-typed

(require plai-typed/s-exp-match)
(print-only-errors true)
(require (typed-in racket [fourth : ((listof 'a) -> 'a)]))

;; Esha Joshi
;; ejoshi@calpoly.edu
;; CPE 430 - 01, Assignment4
;; 05.14.2015

;; Rudimentary Interpreter: Parser and interpreter for the Excr2 language

;; Representation of the language: EXCR2
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
           (body : EXCR2)]
  ;; Able to create new arrays
  [new-array (val : EXCR2) (size : EXCR2)]
  ;; Returns an element of an array
  [ref (arr : EXCR2) (loc : EXCR2)]
  ;; Mutate arrays
  [mut-arrays (arr : EXCR2) (loc : EXCR2) (newVal : EXCR2)]
  ;; Mutate bindings
  [mut-bindings (s : symbol) (newVal : EXCR2)]
  ;; Evaluates a sequence of expressions
  [begin2 (l : (listof EXCR2))])

;; Representation of a Value
(define-type Value
  [numV (n : number)]
  [strV (s : string)]
  [boolV (b : boolean)]
  [cloV (param : (listof symbol))
        (body : EXCR2)
        (env : Env)]
  [arrayV (l : Location) (size : number)])

;; Representation of a Result which returns a value and a store
(define-type Result
  [v*s (v : Value) (s : Store)])

;; Representation of a Result2 which returns an environment and a store
(define-type Result2
  [e*s (env : Env) (s : Store)])

;; Representation of a Binding
;; Binds a name to a Value
(define-type Binding
  [bind (name : symbol) (val : Location)])

(define-type-alias Location number)

;; Environment represented as a listof Bindings
;; Environment maps names to locations
(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

;; Representation of a Storage
;; Maps memory locations (numbers) to values
(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define while 14)
(define in-order 7)

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
         [(s-exp-match? `{if ANY ANY ANY} s) 
          (if (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         ;; Parse fundefC
         [(s-exp-match? `{fn ANY ANY} s)
          (cond
            [(check-param-name (convert-to-symbols (s-exp->list (second sl))))
             (fundefC (map s-exp->symbol (s-exp->list (second sl)))
                      (parse (third sl)))])]
         ;; Parse new-array
         [(s-exp-match? `{new-array ANY ANY} s)
          (new-array (parse (second sl)) (parse (third sl)))]
         ;; Parse ref
         [(s-exp-match? `{ref ANY {ANY}} s)
          (ref (parse (second sl))
               (parse (first (s-exp->list (third sl)))))]
         ;; Parse for mutation of arrays
         [(s-exp-match? `{ANY {ANY} <- ANY} s)
          (mut-arrays (parse (first sl))
               (parse (first (s-exp->list (second sl))))
               (parse (fourth sl)))]
         ;; Parse for mutation of bindings
         [(s-exp-match? `{SYMBOL <- ANY} s)
          (mut-bindings (s-exp->symbol (first sl)) (parse (third sl)))]
         ;; Parse begin2
         [(s-exp-match? `{begin2 ANY ANY ...} s)
          (begin2 (map parse (rest sl)))]
         ;; Parse with
         [(s-exp-match? `{with {ANY = ANY} ... ANY} s)
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
          (funcCall (parse (first sl)) (map parse (rest sl)))]
         ))]
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
(test (parse `{if x x {- x 1}}) (if (idC 'x) (idC 'x) (binOp '- (idC 'x) (numC 1))))
(test (parse `(+ 1 2)) (binOp '+ (numC 1) (numC 2)))
(test (parse `(- 5 3)) (binOp '- (numC 5) (numC 3)))
(test (parse `(fn {x} (+ x x))) (fundefC (list 'x) (binOp '+ (idC 'x) (idC 'x))))
(test (parse `(fn {x y} (+ x y))) (fundefC (list 'x 'y) (binOp '+ (idC 'x) (idC 'y))))
(test (parse `(fn () (- (+ 3 10) (* 2 3)))) 
      (fundefC empty (binOp '- (binOp '+ (numC 3) (numC 10)) (binOp '* (numC 2) (numC 3)))))
(test (parse `{new-array (+ 3 10) 4}) (new-array (binOp '+ (numC 3) (numC 10)) (numC 4)))
(test (parse `{ref p[(+ 0 1)]}) (ref (idC 'p) (binOp '+ (numC 0) (numC 1))))
(test (parse `{p[(+ 0 1)] <- (+ 3 5)}) (mut-arrays (idC 'p) (binOp '+ (numC 0) (numC 1)) (binOp '+ (numC 3) (numC 5))))
(test (parse `{p <- (+ 3 5)}) (mut-bindings 'p (binOp '+ (numC 3) (numC 5))))
(test (parse `{begin2 {+ 3 5} p}) (begin2 (list (binOp '+ (numC 3) (numC 5)) (idC 'p))))

(test (parse `{with {z = {+ 9 14}}
                    {y = 98}
                    {+ z y}})
      (funcCall (fundefC (list 'z 'y) (binOp '+ (idC 'z) (idC 'y))) (list (binOp '+ (numC 9) (numC 14)) (numC 98))))
(test (parse '(g 15)) (funcCall (idC 'g) (list (numC 15))))
(test (parse `((g) 15)) (funcCall (funcCall (idC 'g) empty) (list (numC 15))))
(test (parse `((fn () 9))) (funcCall (fundefC empty (numC 9)) empty))
(test (parse `{(fn () 9) 17}) (funcCall (fundefC empty (numC 9)) (list (numC 17))))
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
;(test/exn (parse '(fn + () 13)) "parse: invalid input")
(test/exn (parse '(- 1 2 3)) "parse: invalid input")
(test/exn (parse `(fn {x x} (numC 3))) "parse: duplicate param names")
#;(test/exn (parse `(with {z = (fn empty 3)} 
                        {z = 9} 
                        {z})) "parse: duplicate param names")

;#############################

;; Function env-lookup : symbol Env -> Location
;; Looks up Binding in an environment
(define (env-lookup [for : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error 'env-lookup (string-append "name not found: " (to-string for)))]
    [else (cond
            [(symbol=? for (bind-name (first env))) (bind-val (first env))]
            [else (env-lookup for (rest env))])]))

(test (env-lookup 'z (list (bind 'xxx 143) (bind 'z 134))) 134)
(test (env-lookup 'esha (list (bind 'binding 69) (bind 'esha 1))) 1)
(test/exn (env-lookup 'noBinding (list (bind 'bindbaby 123))) "name not found: 'noBinding")

;#############################

;; Function fetch : Location Store -> Value
;; Looks up values in the Store
(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch (string-append "Location not found: " (to-string loc)))]
    [else (cond
            [(eq? loc (cell-location (first sto))) (cell-val (first sto))]
            [else (fetch loc (rest sto))])]))

(test (fetch 3 (list (cell 1 (numV 1)) (cell 2 (numV 2)) (cell 3 (numV 3)))) (numV 3))
(test/exn (fetch 153 (list (cell 1 (numV 1)) (cell 2 (numV 2)) (cell 3 (numV 3)))) "Location not found: 153")

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

;; Function interp-binop : symbol Value Value Store -> Result
;; Takes in an operator, 2 Values, and 1 Store and yields a Result
(define (interp-binop [operator : symbol] [v-l : Value] [v-r : Value] [s-r : Store]) : Result
  (cond 
    [(eq? operator '+) (v*s (num+ v-l v-r) s-r)]
    [(eq? operator '-) (v*s (num- v-l v-r) s-r)]
    [(eq? operator '*) (v*s (num* v-l v-r) s-r)]
    [(eq? operator '/) (v*s (num/ v-l v-r) s-r)]
    [(eq? operator '<=) (v*s (num<= v-l v-r) s-r)]
    [(eq? operator 'eq?) (v*s (val-eq v-l v-r) s-r)]))

;#############################

;; Function get-max-loc : Store -> number
;; Finds the max location occupied in the store
(define (get-max-loc [s : Store] [startingMax : number]) : number
  (cond
    [(empty? s) startingMax]
    [(> (cell-location (first s)) startingMax) (get-max-loc (rest s) (cell-location (first s)))]
    [else (get-max-loc (rest s) startingMax)]))

(test (get-max-loc (list (cell 2 (numV 2)) (cell 1 (numV 1))) 1) 2)

;#############################

;; Function new-loc : Store -> number
;; Scans the Store and finds the next available address
(define (new-loc [s : Store]) : number
    (cond
      [(empty? s) 1]
      [else (+ (get-max-loc s 0) 1)]))

(test (new-loc (list (cell 2 (numV 2)) (cell 1 (numV 1)))) 3)
(test (new-loc (list (cell 22 (numV 22)) (cell 21 (numV 21)) (cell 20 (numV 20)) (cell 19 (numV 19)))) 23)
(test (new-loc empty) 1)

;#############################

;; Function add-to-store : Store Value : Store
;; Takes in the initial Store and creates a new Storage-cell with the next available locations and the passed in Value
(define (add-to-store [s : Store] [v : Value]) : Store
  (cond
    [(empty? s) (list (cell 1 v))]
    [else (cons (cell (new-loc s) v) s)]))

(test (add-to-store (list (cell 1 (numV 3))) (numV 3)) 
      (list (cell 2 (numV 3)) (cell 1 (numV 3))))
(test (add-to-store (list) (numV 5)) (list (cell 1 (numV 5))))

;#############################

;; Function allocate : Store number Value -> Result
;; Allocates a new array with specified size and initializes the cells in the Store with a Value
(define (allocate [s : Store] [n : number] [size : number] [val : Value]) : Result
  (cond
    [(> n 0) (allocate (add-to-store s val) (- n 1) size val)]
    [else (v*s (arrayV (- (new-loc s) size) size) s)]))

(test (allocate (list (cell 3 (numV 5)) (cell 2 (numV 5)) (cell 1 (numV 5))) 3 3 (numV 2))
      (v*s (arrayV 4 3) (list (cell 6 (numV 2)) (cell 5 (numV 2)) (cell 4 (numV 2)) 
                     (cell 3 (numV 5)) (cell 2 (numV 5)) (cell 1 (numV 5)))))

;#############################

;; Function begin2-helper : (listof EXCR2) Store Env -> Result
;; Implements store-passing style to evaluate the list of expressions
;; Returns the last expression as a Value-Store pair
(define (begin2-helper [l : (listof EXCR2)] [s : Store] [env : Env]) : Result
  (let ([first (first l)])
    (cond
      [(empty? (rest l)) 
        (interp first env s)]
      [else
       (type-case Result (interp first env s)
         [v*s (v-first s-first)
              (begin2-helper (rest l) (rest s) env)])])))

;#############################

;; Function interp-helper : (listof symbol) (listof EXCR2) Env -> Env
;; Adds bindings to an environment and cells to a store
(define (interp-helper [fdArgs : (listof symbol)] [args : (listof EXCR2)] [env : Env] [cloV : Env]  [s : Store]) : Result2
  (cond
    [(empty? fdArgs) (e*s cloV s)]
    [else
     (let ([res (interp (first args) env s)])
       (interp-helper (rest fdArgs) (rest args) env
                      (cons (bind (first fdArgs) (new-loc s)) cloV) 
                      (cons (cell (new-loc s) (v*s-v res)) s)))]))

;#############################

;; Function interp : EXCR2 Env -> Result
;; Return the store reflecting all mutations that happened while evaluating the given expression
;; numC: Number is a constant -> No mutations could have happened -> Returned store is the same as passed in
(define (interp [e : EXCR2] [env : Env] [sto : Store]) : Result
  (type-case EXCR2 e
    [numC (n) (v*s (numV n) sto)]
    [idC (s) (v*s (fetch (env-lookup s env) sto) sto)]
    [bool (b) (v*s (boolV b) sto)]
    [if (testNum then els)
        (type-case Result (interp testNum env sto)
          [v*s (v-testNum s-testNum)
               (cond
                 [(boolV-b (val-eq (v*s-v (interp testNum env s-testNum)) (boolV #t)))
                  (interp then env s-testNum)]
                 [else (interp els env s-testNum)])])]
    [binOp (operator l r)
           (type-case Result (interp l env sto)
             [v*s (v-l s-l)
                  (type-case Result (interp r env s-l)
                    [v*s (v-r s-r)
                         (interp-binop operator v-l v-r s-r)])])]
    [funcCall (funcDef args)
              (type-case Result (interp funcDef env sto)
                [v*s (v-funcDef s-funcDef) 
                     (cond
                       [(eq? (length args) (length (cloV-param v-funcDef))) 
                        (type-case Result2 (interp-helper (cloV-param v-funcDef) args env (cloV-env v-funcDef) s-funcDef)
                          [e*s (e-args s-args)
                               (interp (cloV-body v-funcDef) e-args s-args)
                               ]
                          )
                        ]
                       [else (error 'parse "wrong number of arguments")]
                       )
                     ]
                )
              ]
    [fundefC (argument body)
             (v*s (cloV argument body env) sto)]
    ;; Creates a fresh array of the given size, with all cells filled with the given value
    [new-array (val size)
               (type-case Result (interp val env sto)
                 [v*s (v-val s-val)
                      (type-case Result (interp size env s-val)
                        [v*s (v-size s-size)
                             (allocate s-val (numC-n size) (numC-n size) v-val)])])]
    ;; Returns an element of an array
    [ref (arr loc)
         (type-case Result (interp arr env sto)
           [v*s (v-arr s-arr)
                (type-case Result (interp loc env s-arr)
                  [v*s (v-loc s-loc)
                       (v*s (fetch (numC-n loc) s-loc) s-loc)])])]
    ;; Mutate arrays
    [mut-arrays (arr loc newVal)
            (type-case Result (interp arr env sto)
              [v*s (v-arr s-arr)
                   (type-case Result (interp loc env s-arr)
                     [v*s (v-loc s-loc)
                          (type-case Result (interp newVal env s-arr)
                            [v*s (v-newVal s-newVal)
                                 (v*s v-newVal (override-store (cell (+ (arrayV-l v-arr) (numV-n v-loc)) v-newVal) s-newVal))])])])]
    ;; Mutate bindings -- change the binding of the symbol to contain the new value
    [mut-bindings (s newVal)
                  (type-case Result (interp newVal env sto)
                    [v*s (v-newVal s-newVal)
                         (v*s v-newVal (override-store (cell (env-lookup s env) v-newVal) s-newVal))])]
    ;; Begin2 -- evaluates a sequence of expressions, returning the last one
    [begin2 (l)
           (let ([first (first l)])
             (type-case Result (interp first env sto)
               [v*s (v-first s-first)
                    (begin2-helper (rest l) s-first env)]))]
    ;[else (error 'interp "unimplemented")]
    )
  )

;(define-type Result
;  [v*s (v : Value) (s : Store)])

;(define-type Result2
;  [e*s (env : Env) (s : Store)])

;(define-type EXCR2
;  [numC (n : number)]
;  [idC (s : symbol)]
;  [bool (b : boolean)]
;  [if (if : EXCR2) (then : EXCR2) (els : EXCR2)]
;  [binOp (operator : symbol) (l : EXCR2) (r : EXCR2)]
;  [funcCall (funcDef : EXCR2) 
;            (arguments : (listof EXCR2))]
;  [fundefC (args : (listof symbol))
;           (body : EXCR2)]
;  [new-array (val : EXCR2) (size : EXCR2)]
;  [ref (arr : EXCR2) (loc : EXCR2)]
;  [mut-arrays (arr : EXCR2) (loc : EXCR2) (newVal : EXCR2)]
;  [mut-bindings (s : symbol) (newVal : EXCR2)]
;  [begin2 (list : (listof EXCR2))]
;  )

;(define-type Value
;  [numV (n : number)]
;  [strV (s : string)]
;  [boolV (b : boolean)]
;  [cloV (param : (listof symbol))
;        (body : EXCR2)
;        (env : Env)]
;  [arrayV (l : Location) (size : number)])

;(define-type Storage
;  [cell (location : Location) (val : Value)])

;(define-type-alias Store (listof Storage))

;(define-type Binding
;  [bind (name : symbol) (val : Location)])

;; Library of Bindings aka Environment
(define MY-BINDINGS
  (list (bind 'a 10)
        (bind 'esha 13)
        (bind 'someBinding 15)))

;; Library of Storages aka Store
(define MY-STORE
  (list (cell 1 (numV 1)) 
        (cell 15 (numV 15))
        (cell 25 (boolV #t))))

;; Test Case for begin2-helper
(test (begin2-helper (list (numC 4) (numC 3) (bool #t)) (list (cell 3 (numV 5)) (cell 2 (numV 5)) (cell 1 (numV 5))) empty)
      (v*s (boolV #t) (list (cell 1 (numV 5)))))

;; Test Case for interp-helper
(test (interp-helper (list 'x 'y 'z) (list (numC 1) (numC 2) (numC 3)) empty empty (list (cell 1 (numV 1)) (cell 2 (numV 2)) (cell 3 (numV 3))))  
      (e*s (list (bind 'z 6) (bind 'y 5) (bind 'x 4)) 
           (list (cell 6 (numV 3)) (cell 5 (numV 2)) (cell 4 (numV 1)) (cell 1 (numV 1)) (cell 2 (numV 2)) (cell 3 (numV 3)))))

;; Test Cases for interp
(test (interp (numC 10) (list (bind 'binding 10)) MY-STORE) (v*s (numV 10) MY-STORE))
(test (interp (idC 'esha) (list (bind 'xy 10) (bind 'esha 11)) (list (cell 11 (numV 5)) (cell 10 (numV 5)))) 
      (v*s (numV 5) (list (cell 11 (numV 5)) (cell 10 (numV 5)))))
(test (interp (idC 'progLangs) (list (bind 'progLangs 10) (bind 'esha 11)) (list (cell 11 (numV 5)) (cell 10 (numV 4)))) 
      (v*s (numV 4) (list (cell 11 (numV 5)) (cell 10 (numV 4)))))
(test (interp (bool #f) (list (bind 'progLangs 10) (bind 'esha 11)) (list (cell 11 (numV 5)) (cell 10 (numV 4))))
      (v*s (boolV #f) (list (cell 11 (numV 5)) (cell 10 (numV 4)))))
(test (interp (bool #f) (list (bind 'someBinding 34)) empty)
      (v*s (boolV #f) empty))
(test (interp (if (bool #t) (binOp '* (numC 5) (numC 6)) (binOp '/ (numC 10) (numC 2))) 
              (list (bind 'progLangs 10) (bind 'esha 11)) (list (cell 11 (numV 5)) (cell 10 (numV 4)))) 
      (v*s (numV 30) (list (cell 11 (numV 5)) (cell 10 (numV 4)))))
(test (interp (if (bool #f) (binOp '* (numC 5) (numC 6)) (binOp '/ (numC 10) (numC 2))) empty empty) 
      (v*s (numV 5) empty))
(test (interp (binOp '+ (numC 5) (numC 6)) (list (bind 'binding 10)) MY-STORE) (v*s (numV 11) MY-STORE))
(test (interp (binOp '- (numC 5) (numC 6)) (list (bind 'binding2 5)) MY-STORE) (v*s (numV -1) MY-STORE))
(test (interp (binOp '<= (numC 5) (numC 6)) (list (bind 'binding3 1)) MY-STORE) (v*s (boolV #t) MY-STORE))
(test (interp (binOp 'eq? (numC 5) (numC 6)) (list (bind 'binding3 1)) MY-STORE) (v*s (boolV #f) MY-STORE))
(test (interp (funcCall (fundefC (list 'x 'y) (binOp '+ (idC 'x) (idC 'y))) (list (numC 5) (numC 6))) empty empty)
      (v*s (numV 11) (list (cell 2 (numV 6)) (cell 1 (numV 5)))))
(test (interp (funcCall (fundefC (list) (numC 9)) (list)) (list) (list)) (v*s (numV 9) empty))

;(test (interp (funcCall (fundefC empty (numC 9)) (list (numC 17)))) (v*s (numV 4) empty))
;(bad (interp (appC (fundefC (list) (num 9)) (list)) (list) (list)) (v*s (numV 9) '()) (v*s (numV 3) '()) "at line 174")

(test (interp (new-array (numC 5) (numC 4)) empty empty)
              (v*s (arrayV 1 4) (list (cell 4 (numV 5)) (cell 3 (numV 5)) (cell 2 (numV 5)) (cell 1 (numV 5)))))
(test (interp (new-array (numC 2) (numC 4)) empty (list (cell 2 (numV 6)) (cell 1 (numV 6))))
              (v*s (arrayV 3 4) (list (cell 6 (numV 2)) (cell 5 (numV 2)) (cell 4 (numV 2)) (cell 3 (numV 2)) (cell 2 (numV 6)) (cell 1 (numV 6)))))
(test (interp (ref (numC 2) (numC 2)) empty (list (cell 2 (numV 6)) (cell 1 (numV 6))))
              (v*s (numV 6) (list (cell 2 (numV 6)) (cell 1 (numV 6)))))
(test (interp (mut-arrays (new-array (numC 2) (numC 4)) (numC 2) (numC 4)) empty (list (cell 2 (numV 6)) (cell 1 (numV 6))))
      (v*s (numV 4) (list (cell 5 (numV 4)) (cell 6 (numV 2)) (cell 5 (numV 2)) (cell 4 (numV 2)) (cell 3 (numV 2)) (cell 2 (numV 6)) (cell 1 (numV 6)))))
(test (interp (mut-bindings 'p (numC 5)) (list (bind 'p 12)) (list (cell 2 (numV 6)) (cell 1 (numV 6))))
     (v*s (numV 5) (list (cell 12 (numV 5)) (cell 2 (numV 6)) (cell 1 (numV 6)))))
(test (interp (begin2 (list (numC 5) (numC 1) (binOp '+ (numC 1) (numC 2)))) (list) (list (cell 1 (numV 1))))
      (v*s (numV 3) empty))

;(define (interp [e : EXCR2] [env : Env] [sto : Store]) : Result

;(define-type Result
;  [v*s (v : Value) (s : Store)])

;;--- FIX ---
;(test/exn (interp (idC 'a) (list (bind 'binding 10)) MY-STORE) "interp: unimplemented")
;;-----------
(test/exn (interp (ref (numC 0) (numC 2)) empty empty)
              "fetch: Location not found: 2")
(test/exn (interp (ref (numC 3) (numC 6)) empty  (list (cell 3 (numV 3)) (cell 2 (numV 6)) (cell 1 (numV 6))))
              "fetch: Location not found: 6")

;#############################

;; Function top-eval : s-expression -> string
(define (top-eval [s : s-expression]) : string
  (serialize (v*s-v (interp (parse s) empty empty))))

(test (top-eval `(+ 4 1)) "5")
(test (top-eval `(- 7 3)) "4")
(test (top-eval `(* 5 8)) "40")
(test (top-eval `(/ 60 12)) "5")
(test (top-eval `((fn (minus) (minus 8 5)) (fn (a b) (+ a (* -1 b))))) "3")
(test/exn (top-eval `((fn () 9) 17)) "parse: wrong number of arguments")

;while evaluating 
; (top-eval (quote ((fn (seven) (seven)) ((fn (minus) (fn () (minus (+ 3 10) (* 2 3)))) (fn (x y) (+ x (* -1 y)))))))

;  parse: wrong number of arguments
;Saving submission with errors.
