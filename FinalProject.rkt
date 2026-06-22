#lang eopl

;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression> {<expression>]* end
;;                      <a-program (exp exps)>
;;  <expression>    ::= <identifier>
;;                      id-exp (id)
;;                  ::= "<text>"
;;                      cadena (text)
;;                  ::= <number>
;;                      numero (datum)
;;                  ::= <bool>
;;                      numero (datum)
;;                  ::= None
;;                      null ()
;;                  ::= var <identifier> = <expression> {<identifier> = <expression>}*(,);
;;                      var-exp (id exp)
;;                  ::= const <identifier> = <expression>;
;;                      const-exp (id exp)
;;                  ::= (<expression> <primitive-bin> <expression>)
;;                      <primapp-bin-exp (rand1 prim rand2)>
;;                  ::= <primitive-un> (<expression>)
;;                      <primapp-un-exp (prim rand)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {<identifier> = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= func({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= <expression> ({<expression>}*)
;;                      <app-exp (proc rands)>
;;                  ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identifier> = <expression>
;;                     <set-exp (id rhsexp)>
;;                  ::= print ( <expression> );
;;                     <print-exp (exp)>
;;  <primitive-bin> ::= + | - | * | /
;;                  ::= < | > | <= | >= | == | !=
;;                  ::= and | or
;;  <primitive-un>  ::= add1 | sub1
;;                  ::= not
;;  <bool>          ::= true | false


;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("#" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  ;; Números flotantes
  (number
   (digit "." (arbno digit)) number)
  (number
   ("-" digit "." (arbno digit)) number)
  ;; Cadenas de caracteres
  (string-text
   ("\"" (arbno (not #\")) "\"") string)
  ;; (string-text (letter (arbno (or letter digit))) symbol)
  ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression (arbno expression) "end") a-program)
    ;; Datos inmutables
    (expression (identifier) id-exp)
    (expression (string-text) cadena-exp)
    (expression (number) numero-exp)
    (expression ("null") null-exp)

    ;; Declaraciones secuenciales
    
    (expression ("var" (separated-list identifier "=" expression ",")";") var-exp)
    (expression ("const" (separated-list identifier "=" expression ",")";") const-exp)
    (expression ("set" identifier "=" expression ";") set-exp)
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("(" expression primitive-bin expression ")")
                primapp-bin-exp)
    (expression (primitive-un  "(" expression ")")
                primapp-un-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("func" "(" (arbno identifier) ")" expression)
                proc-exp)
    (expression ("[" expression "("(arbno expression) "]")
                app-exp)
    (expression ("print" "(" expression ")" ";") print-exp)
    ;; Booleanos
    (expression (bool) boolean-exp)
    (bool ("true") true-exp)
    (bool ("false") false-exp)
    ;; Estructuras de control
    ;(expression ("if" expression "then" expression "else" expression "end") if-exp)
    ;(expression ("begin" (separated-list expression ";") "end") begin-exp)
    ;(expression ("switch" expression "{" (arbno "case" expression ":" expression) "default" ":" expression "}") switch-exp)
    ;(expression ("while" expression "do" expression "done") while-exp)
    ;(expression ("for" identifier "in" expression "do" expression "done")for-exp)

    ;; Paradigma funcional
    ;; Por ahora, pero falta mejorar la definición de funciones
    ;(expression ("func" identifier "(" (separated-list identifier ",") ")" "{" expression "}") func-exp)
    ;(expression ("return" expression) return-exp)
    ;(expression ("call" "(" expression (arbno "," expression) ")") app-exp)

    ;; Estructuras mutables
    ; Listas: [1, 2, 3]
    ;(expression ("[" (separated-list expression ",") "]") list-exp)
    ; Diccionarios: { "a": 1, "b": 2 }
    ;(expression ("{" (separated-list identifier ":" expression ",") "}") dict-exp)


    ;; Evaluación de expresiones algebraicas
    (expression ("evaluar" "(" expression "," (separated-list identifier "=" expression ",") ")")
                eval-exp)
    (primitive-bin ("+") add-prim)
    (primitive-bin ("-") substract-prim)
    (primitive-bin ("*") mult-prim)
    (primitive-bin ("/") div-prim)
    (primitive-bin (">") greater-prim)
    (primitive-bin ("<") less-prim)
    (primitive-bin (">=") greater-equal-prim)
    (primitive-bin ("<=") less-equal-prim)
    (primitive-bin ("==") equal-prim)
    (primitive-bin ("!=") not-equal-prim)
    (primitive-bin ("and") and-prim)
    (primitive-bin ("or") or-prim)
;; Unary primitives
    (primitive-un ("add1") incr-prim)
    (primitive-un ("sub1") decr-prim)
    (primitive-un ("not") not-prim)
    ;(primitive ("simplificar") simplify-prim)
    ))


;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)


(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp exps)
                 (letrec
                     ([env (init-env)]
                      [loop (lambda (acc exps current-env)
                              (if (null? exps)
                                  acc
                                  (let
                                      ([result (eval-expression (car exps) current-env)])
                                    (if (environment? result)
                                        (loop 1 (cdr exps) result)
                                        (loop result (cdr exps) current-env)))
                                  )
                              )
                            ])
                   (let
                       ([result (eval-expression exp env)])
                     (if (environment? result)
                         (loop 1 exps result)
                         (loop result exps env)))
                   )
                   )
                 )
      )
    )


; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))

(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-env))))

;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z f)
;     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) ())) ())))
;                      (empty-env)))
;     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

;**************************************************************************************
;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?))
  (readonly-target (expval expval?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;**************************************************************************************

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (numero-exp (datum) datum)
      (id-exp (id) (apply-env env id))
      (cadena-exp (txt) (substring txt 1 (- (string-length txt) 1)))

      (var-exp (ids rhs-exps)
               (let ((vals (map (lambda (e) (eval-expression e env)) rhs-exps)))
                 (extend-env ids (map direct-target vals) env)))
 
      (const-exp (ids rhs-exps)
                 (let ((vals (map (lambda (e) (eval-expression e env)) rhs-exps)))
                   (extend-env ids (map readonly-target vals) env)))
     
      (set-exp (id rhs-exp)
               (let ((val (eval-expression rhs-exp env))
                     (ref (apply-env-ref env id)))
                 (setref! ref val)
                 val))

      (null-exp () 'null)
      (primapp-bin-exp (rand1 prim rand2)
                       ; Evalua cada de sus expresiones
                   (let ((arg1 (eval-expression rand1 env)) 
                         (arg2 (eval-expression rand2 env)))
                     ; Aplica la primitiva con el resultado de evaluar sus expresiones
                     (apply-primapp-bin arg1 prim arg2)))
      (primapp-un-exp (prim exp)
                      ; Aplica la primitiva unaria recibida con el resultado de evaluar la expresión
                      (apply-primapp-un prim (eval-expression exp env))) 
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (begin-exp (exp exps)
                 (let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expression (car exps) 
                                               env)
                              (cdr exps)))))
      (boolean-exp (exp)
                   (eval-bool exp))
      (print-exp (exp)
                 (display (eval-expression exp env))
                 (newline)
                 1)
      (else exp))))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (id-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (readonly-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (eval-expression rand env))))))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env))
         rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primapp-bin
  (lambda (exp1 prim exp2)
    ; Cases para aplicar según la variante de primitiva binaria
    (cases primitive-bin prim
      ; Primitiva suma
      ; Suma ambas expresiones
      (add-prim () (+ exp1 exp2))
      ; Primitiva resta
      ; Resta ambas expresiones
      (substract-prim () (- exp1 exp2))
      ; Primitiva división
      ; Divide las expresiones
      (div-prim () (/ exp1 exp2))
      ; Primitiva multiplicación
      ; Multiplica las expresiones
      (mult-prim () (* exp1 exp2))
      (greater-prim () (> exp1 exp2))
      (less-prim () (< exp1 exp2))
      (greater-equal-prim () (>= exp1 exp2))
      (less-equal-prim () (<= exp1 exp2))
      (equal-prim () (equal? exp1 exp2))
      (not-equal-prim () (not(equal? exp1 exp2)))
      (and-prim () (and (true-value? exp1) (true-value? exp2)))
      (or-prim () (or (true-value? exp1) (true-value? exp2)))
      )
    )
  )

(define apply-primapp-un
  (lambda (prim exp)
    ; Cases para aplicar según la variante de primitiva unaria
    (cases primitive-un prim
    ; Primitiva sumar-1
    (incr-prim () (if (number? exp) ; Chequea que la expresión sea un numero
                           ; Suma 1 a la expresión
                           (+ exp 1)
                           ; Sino retorna un error
                           (eopl:error 'contract-violation "La expresión no es un número: ~s" exp)))
     ; Primitiva restar-1
    (decr-prim () (if (number? exp) ; Chequea que la expresión sea un numero
                           ; Resta 1 a la expresión
                           (- exp 1)
                           ; Sino, retorna error
                           (eopl:error 'contract-violation "La expresión no es un número: ~s" exp)))
      (not-prim () (not(true-value? exp)))
      )
    )
  )

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (cond
      ((bool? x) (true-value? (eval-bool x)))
      ((eqv? x #f) #f)
      ((eqv? x 0) #f)           
      ((equal? x "") #f)        
      ((eqv? x 'null) #f)      
      (else #t))))


;eval-bool: Evalua las expresiones booleanas true o false al true y false de racktet
(define eval-bool
  (lambda (exp)
    (cases bool exp
      (true-exp () #t)
      (false-exp () #f)
      )
    )
  )

;bool->text: Transforma el resultado de una evaluación booleana de Racket a texto "true" o "false"
(define bool->text
  (lambda (exp)
    (if exp
    "true"
    "false"))
  )

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env))))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    ;(begin
     ; (display env)
      ;(display "jajajaj ")
      (deref (apply-env-ref env sym))))
    ;)

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    ;agregado symbol a expval
    (or (number? x) (procval? x) (symbol? x) (string? x) (boolean? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (readonly-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (readonly-target (expval) expval) ; lee el valor igual q direct-target
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (readonly-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref
          (cases target (primitive-deref ref)
            (direct-target (expval1) ref)
            (readonly-target (expval1) 
                             (eopl:error 'setref! "Error: Intento de modificar una constante inmutable."))
            (indirect-target (ref1)
                             (cases target (primitive-deref ref1)
                               (readonly-target (v) 
                                                (eopl:error 'setref! "Error: Intento de modificar una constante inmutable."))
                               (else ref1))))))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************************************************************************
;Pruebas

(show-the-datatypes)
just-scan
scan&parse
(just-scan "add1(x) end")
(just-scan "add1(   x   )#cccc end")
(just-scan "add1(  +(5, x)   )#cccc end")
(just-scan "add1(  +(5, #ccccc x)  end")
(scan&parse "add1(x) end")
(scan&parse "add1(   x   ) end")
(scan&parse "add1((x + 5)) end")
(scan&parse "add1(  (5 + #cccc
x))  end")
(scan&parse "if (x - 4) then (y + 11) else (y * 10) end")

(just-scan "add1(  (5 + #esto es un comentario \n x)) end")
(scan&parse "add1(  (5 + #esto es un comentario \n x)) end")

(define caso1 (primapp-un-exp (incr-prim) (numero-exp 5)))
(define exp-numero (numero-exp 8))
(define exp-ident (id-exp 'c))
(define exp-app (primapp-bin-exp exp-numero (add-prim) exp-ident))
(define programa (a-program exp-app '()))
(define una-expresion-dificil (primapp-bin-exp
                               (primapp-un-exp (incr-prim) (id-exp 'v))
                               (mult-prim)
                               (numero-exp 200)))
(define un-programa-dificil
    (a-program una-expresion-dificil '()))


(interpretador)

; var abc = 42;
;begin
;  set abc = "Ahora soy un texto";
;  set abc = 20;
;  abc
;end

