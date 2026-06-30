#lang eopl
(require (only-in racket 
                  hash? 
                  hash 
                  make-hash 
                  hash-ref 
                  hash-set 
                  hash-set! 
                  hash-has-key? 
                  hash-keys 
                  hash-values))

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
;;                  ::= func({<identifier>}*(,)) { {<expression>}* }
;;                      <proc-exp (ids body)>
;;                  ::= def <identifier>({<identifier>}*(,)) { {<expression>}* }
;;                      <def-exp (name ids body)>
;;                  ::= return <expression>;
;;                      <return-exp (return-exp)>
;;                  ::= crear-diccionario({<expression> : <expression>}*(,))
;;                      <dict-exp (keys values)>
;;                  ::= [<expression> ({<expression>}*)]
;;                      <app-exp (proc rands)>
;;                  ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identifier> = <expression>
;;                     <set-exp (id rhsexp)>
;;                  ::= print ( <expression> );
;;                     <print-exp (exp)>
;;                  ::= crear-diccionario ( {<expression> : <expression>}*(,) )
;;                      <dict-exp (keys values)>
;;                  ::= symbol <identifier>;
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
    (expression ("vacio") vacio-exp)

    ;; Declaraciones secuenciales
    (expression ("var" (separated-list identifier "=" expression ",")";") var-exp)
    (expression ("const" (separated-list identifier "=" expression ",")";") const-exp)
    (expression ("set" identifier "=" expression ";") set-exp)
    (expression ("(" expression primitive-bin expression ")")
                primapp-bin-exp)
    (expression (primitive-bin "(" expression "," expression ")")
                primapp-prefix-bin-exp)
    (expression (primitive-un  "(" expression ")")
                primapp-un-exp)
    (expression (primitive-ter "(" expression "," expression "," expression ")")
                primapp-ter-exp)
    
    (expression ("print" "(" expression ")" ";") print-exp)

    ;funciones
    (expression ("func" "(" (separated-list identifier ",") ")"  "{" (arbno expression) "}") proc-exp)
    (expression ("def" identifier "(" (separated-list identifier ",") ")"  "{" (arbno expression) "}") def-exp)
    (expression ("return" expression ";") return-exp)
    (expression ("[" expression "("(arbno expression) ")" "]") app-exp)
    
    ;; Booleanos
    (expression (bool) boolean-exp)
    (bool ("true") true-exp)
    (bool ("false") false-exp)
    
    ;; Estructuras de control
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    ;(expression ("begin" (separated-list expression ";") "end") begin-exp)
    (expression ("switch" expression "{" (arbno "case" expression ":" expression) "default" ":" expression "}") switch-exp)
    (expression ("while" expression "do" expression "done") while-exp)
    (expression ("for" identifier "in" expression "do" expression "done")for-exp)

    ;; Paradigma funcional
    ;; Por ahora, pero falta mejorar la definición de funciones
    ;(expression ("func" identifier "(" (separated-list identifier ",") ")" "{" expression "}") func-exp)
    ;(expression ("return" expression) return-exp)

    ;; Estructuras mutables
    ; Listas: [1, 2, 3]
    (expression ("list" "(" (separated-list expression ",") ")") list-exp)
    ; Diccionarios: { "a": 1, "b": 2 }
    ;(expression ("{" (separated-list identifier ":" expression ",") "}") dict-exp)
    (expression ("crear-diccionario" "(" (separated-list expression ":" expression ",") ")") dict-exp)
    ; Simbolos de expresiones algebraicas
    (expression ("symbol" identifier ";") symbol-exp)

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
    (primitive-bin ("%") mod-prim)
    ; String primitive
    (primitive-bin ("concat") concat-prim)
    ; List primitives
    (primitive-bin ("crear-lista") crear-lista-prim)
    (primitive-bin ("append") append-prim)
    (primitive-bin ("ref-list") ref-list-prim)

    ;Dictionaries primitives
    (primitive-bin ("ref-diccionario") ref-dict-prim)
    
    ;; Unary primitives
    (primitive-un ("add1") incr-prim)
    (primitive-un ("sub1") decr-prim)
    (primitive-un ("not") not-prim)
    ; String primitive
    (primitive-un ("longitud") length-prim)
    ; List primitives
    (primitive-un ("vacio?") is-vacio-prim)
    (primitive-un ("lista?") is-list-prim)
    (primitive-un ("cabeza") cabeza-prim)
    (primitive-un ("cola") cola-prim)
    ;(primitive ("simplificar") simplify-prim)

    ; Dictionaries primitives
    (primitive-un ("diccionario?") is-dict-prim)
    (primitive-un ("claves") dict-keys-prim)
    (primitive-un ("valores") dict-values-prim)

    (primitive-ter ("set-list") set-list-prim)

    ; Dictionaries primitives
    (primitive-ter ("set-diccionario") set-dict-prim)
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

(define-datatype return-signal return-signal?
  (a-return
   (val expval?)))

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
      (vacio-exp () (vector)) ; Retorna un vector de tamaño cero #()
      (primapp-bin-exp (rand1 prim rand2)
                       ; Evalua cada de sus expresiones
                       (let ((arg1 (eval-expression rand1 env)) 
                         (arg2 (eval-expression rand2 env)))
                       ; Aplica la primitiva con el resultado de evaluar sus expresiones
                       ; Si algún argumento evalua a una expresión simbólica, genera una expresión simbólica
                         (if (or (symb-exp? arg1) (symb-exp? arg2))
                             (let
                                 ((arg1 (if (number? arg1) (symb-num arg1) arg1))
                                  (arg2 (if (number? arg2) (symb-num arg2) arg2)))
                               (symb-op prim arg1 arg2 exp))
                             (apply-primapp-bin arg1 prim arg2))))
      (primapp-prefix-bin-exp (prim rand1 rand2)
                              (let ((arg1 (eval-expression rand1 env))
                                    (arg2 (eval-expression rand2 env)))
                                (apply-primapp-bin arg1 prim arg2)))
      (primapp-un-exp (prim exp)
                      ; Aplica la primitiva unaria recibida con el resultado de evaluar la expresión
                      (apply-primapp-un prim (eval-expression exp env)))
      (primapp-ter-exp (prim rand1 rand2 rand3)
                       (let ((arg1 (eval-expression rand1 env))
                             (arg2 (eval-expression rand2 env))
                             (arg3 (eval-expression rand3 env)))
                         (apply-primapp-ter prim arg1 arg2 arg3)))
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
                 (display (let
                              ((exp-val (eval-expression exp env)))
                            (if (symb-exp? exp-val)
                                (symb-exp->text exp-val)
                                exp-val)
                            ))
                 (newline)
                 1)
      (switch-exp (base-exp cases-exps bodies-exps default-exp)
                  (let ((base-val (eval-expression base-exp env)))
                    (let loop ((cases cases-exps)
                               (bodies bodies-exps))
                      (if (null? cases)
                          (eval-expression default-exp env)
                          (let ((case-val (eval-expression (car cases) env)))
                            (if (equal? base-val case-val)
                                (eval-expression (car bodies) env)
                                (loop (cdr cases) (cdr bodies))))))))
      (list-exp (exps)
                (let ((vals (map (lambda (e) (eval-expression e env)) exps)))
                  (list->vector vals)))
      (while-exp (test-exp body-exp)
                 (let loop () ; Iniciamos un ciclo recursivo infinito
                   (if (true-value? (eval-expression test-exp env)) ; Evaluamos la condición
                       (begin
                         (eval-expression body-exp env)             ; Si es true, entonces evaluamos el cuerpo
                         (loop))                                    ; Luego se vuelve a llamar al ciclo
                       'null)))
      (for-exp (id list-exp body-exp)
               (let ((lst (eval-expression list-exp env)))          ; Evaluamos la lista iterable
                 (if (vector? lst)                                  
                     (let loop ((i 0))                              
                       (if (< i (vector-length lst))               
                           (begin
                             ; Evaluamos el cuerpo en un amb. extendido
                             (eval-expression body-exp 
                                              (extend-env (list id) 
                                                          (list (direct-target (vector-ref lst i))) 
                                                          env))
                             (loop (+ i 1)))                       
                           'null))                                  
                     (eopl:error 'eval-expression "Error: el ciclo for exige una lista, recibio: ~s" lst))))
      (dict-exp (keys values)
                (let ((eval-keys (map (lambda (k) (eval-expression k env)) keys))
                      (eval-values (map (lambda (v) (eval-expression v env)) values)))
                     (make-hash (map cons eval-keys eval-values))))
      (return-exp (ret-exp) (a-return (eval-expression ret-exp env)))
      (def-exp (name ids body) (extend-env-recursively 
                                  (list name)
                                  (list ids)
                                  (list body)
                                env))
      (symbol-exp (id)
                  (extend-env (list id) (list (direct-target (symb-var id))) env))
      (else exp))))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      ;si el argumento es un identificador miramos cual valor tiene
      (id-exp (id)
              (let* ((ref (apply-env-ref env id))
                     (val (deref ref)))
                (if (or (vector? val) (hash? val))
                    ;listas y diccionarios se pasan por referencia
                    (indirect-target ref)
                    ;numeros, cadenas, booleanos, null y funciones porf valor
                    (direct-target val))))
      ;  si el arg no es un id se evalua normalmente 
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
      (mod-prim () (modulo exp1 exp2))
      (concat-prim () (string-append exp1 exp2))
      (crear-lista-prim ()
                        (if (vector? exp2) 
                            (list->vector (cons exp1 (vector->list exp2)))
                            (eopl:error 'apply-primapp-bin 
                                        "Error: El segundo argumento de crear-lista debe ser una lista, se recibio: ~s" exp2)))
      (append-prim ()
                   (if (and (vector? exp1) (vector? exp2))
                       (list->vector (append (vector->list exp1) (vector->list exp2)))
                       (eopl:error 'apply-primapp-bin "Error: append requiere dos listas")))
      (ref-list-prim ()
                     (if (and (vector? exp1) (integer? exp2))
                         (if (and (>= exp2 0) (< exp2 (vector-length exp1)))
                             (vector-ref exp1 exp2) 'null)
                         (eopl:error 'apply-primapp-bin "Error: ref-list requiere una lista y un numero entero")))
      (ref-dict-prim () 
                  (if (hash? exp1)
                      (hash-ref exp1 exp2 'null)
                      (eopl:error 'apply-primapp-bin "Error: ref-diccionario requiere un diccionario como primer argumento, recibio: ~s" exp1)))
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
      (length-prim () (string-length exp))
      (is-vacio-prim ()
                     (if (vector? exp)
                         (= (vector-length exp) 0) #f))
      (is-list-prim () (vector? exp))
      (cabeza-prim ()
                   (if (vector? exp)
                       (if (> (vector-length exp) 0)
                           (vector-ref exp 0) 'null)             
                       (eopl:error 'apply-primapp-un "Error: cabeza requiere una lista")))
      (cola-prim ()
                 (if (vector? exp)
                     (if (> (vector-length exp) 0)
                         (list->vector (cdr (vector->list exp))) (vector))          
                     (eopl:error 'apply-primapp-un "Error: cola requiere una lista")))
      (is-dict-prim () (hash? exp))
      (dict-keys-prim ()
                  (if (hash? exp)
                      (list->vector (hash-keys exp))
                      (eopl:error 'apply-primapp-un "Error: claves requiere un diccionario, recibio: ~s" exp)))
      (dict-values-prim ()
                  (if (hash? exp)
                      (list->vector (hash-values exp))
                      (eopl:error 'apply-primapp-un "Error: valores requiere un diccionario, recibio: ~s" exp)))
      )
    )
  )

(define apply-primapp-ter
  (lambda (prim exp1 exp2 exp3)
    (cases primitive-ter prim
      ; Primitiva set-list(lst, index, valor)
      (set-list-prim ()
                     (if (and (vector? exp1) (integer? exp2))
                         ; Validamos que el índice exista
                         (if (and (>= exp2 0) (< exp2 (vector-length exp1)))
                             (begin
                               (vector-set! exp1 exp2 exp3) ; Modifica
                               exp1)                        ; Retorna la lista
                             (eopl:error 'apply-primapp-ter "Error: Indice ~s fuera de rango" exp2))
                         (eopl:error 'apply-primapp-ter "Error: set-list requiere una lista y un indice entero")))
      (set-dict-prim ()
                      (if (hash? exp1)
                          (begin (hash-set! exp1 exp2 exp3) exp1)
                          (eopl:error 'apply-primapp-ter "Error: set-diccionario requiere un diccionario como primer argumento, recibio: ~s" exp1)))
    )
  )
)


;*******************************************************************************************

; Funciones Auxiliares

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

;is-comparable?: determina si un valor es comparable
(define is-comparable?
  (lambda (exp)
    (or (number? exp) (symbol? exp) (string? exp))
    ))


;eval-bool: Evalua las expresiones booleanas "true" y "false" al #t y #f de racktet
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

;is-symbol-exp?: Determina si una expresión es una expresión simbólica 
(define is-symbol-exp?
  (lambda (exp)
    (cases expression exp
      (symbol-exp (id) #t)
      (else #f))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body (list-of expression?))
   (env environment?)))

#|apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente. 
Si tiene return devuelve el valor de la expresion return, de otra manera devuelve 'null
|#
(define apply-procedure
  ;recibe una closure y los argumentos
  (lambda (proc args)
    (cases procval proc
      ;abrimos la closure 
      (closure (ids body env)
              ;creamos un nuevo ambiente haciendo binding de los argumentos a los ids
               (let ((new-env (extend-env ids args env)))
                ; iteramos sobre el cuerpo de la funcion
                 (let loop ((exps body))
                   ; si se acabo el cuerpo y nunca aparecio return devolvemos 'null
                   (if (null? exps)
                       'null
                       ;evaluamos el cadr en el nuevo ambiente
                       (let ((result (eval-expression (car exps) new-env)))
                         (if (return-signal? result)
                             (cases return-signal result
                               (a-return (val) val))
                             (loop (cdr exps)))))))))))

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
      (deref (apply-env-ref env sym))))

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
    ;agregado symbol y vector a expval, tambien se agrego hash 
    (or (number? x) (procval? x) (symbol? x) (string? x) (boolean? x) (vector? x) (hash? x) (symb-exp? x))))

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

;*******************************************************************************************
;Expresiones Simbólicas

(define-datatype symb-exp symb-exp?
  (symb-var (id symbol?))
  (symb-num (datum number?))
  (symb-op (rand primitive-bin?)
           (rator1 symb-exp?)
           (rator2 symb-exp?)
           (org-exp expression?)
           )
  )

(define symb-exp->text
  (lambda (exp)
    (cases symb-exp exp
      (symb-var (id) (symbol->string id))
      (symb-num (datum) (number->string datum))
      (symb-op  (rand rator1 rator2 exp)
                (string-append "("
                               (symb-exp->text rator1)
                               (primapp-bin->text rand)
                               (symb-exp->text rator2)
                               ")"
                               )
                ))))


(define primapp-bin->text
  (lambda (prim)
    ; Cases para aplicar según la variante de primitiva binaria
    (cases primitive-bin prim
      ; Primitiva suma
      (add-prim () "+")
      ; Primitiva resta
      (substract-prim () "-")
      ; Primitiva división
      (div-prim () "/")
      ; Primitiva multiplicación
      (mult-prim () "*")
      (else "")
      )
    )
  )

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

; ((5 * 4) + (10 / 2)) end
; (true and (5 > 2)) end
; var x = 10, y = 5; const z = 2; ((x + y) * z) end

; var x = 42; set x = "Ahora soy un texto"; set x = 20; x end
; var x1 = 10, x2 = 20, x3 = 30; ((x1 + x2) + x3) end

; var edad = 20; if (edad >= 18) then "Eres mayor de edad" else "Eres menor" end

; var miSuma = func (a b) (a + b); [miSuma (10 20)] end

; Lo dejamos así o le quitamos el ; al set?
; var x = 5; begin set x = 20; ; x end end
; const c = 10; begin set c = 99; ; c end end

; var test1 = 0, test2 = "null", test3 = "Hola"; if test1 then 1 else 2 end
; var test1 = 0, test2 = "null", test3 = "Hola"; if test3 then 1 else 2 end

; var x = 2; switch x { case 1 : "Uno" case 2 : "Dos" default : "Ninguno" } end
;  var color = "rojo"; switch color { case "rojo": print("Detente"); case "amarillo": print("Precaución"); case "verde": print("Sigue"); default: print("Color desconocido"); } end

; (10 % 3) end
; ("Hola " concat "Mundo") end
; longitud("MathFlow") end

; var x = 10; var y = 5; if (x > y) then print(true); else print(false); end
; var a = 10; var b = 5; if ((a > b) and not((b == 0))) then print("a es mayor y b no es cero"); else null end

; var miLista = list(1, "Hola", true, (5 * 2)); print(miLista); end

; var lista = vacio; print(vacio?(lista)); print(vacio?(crear-lista(1, vacio))); end
; var lista = crear-lista(3, vacio); set lista = crear-lista(2, lista); set lista = crear-lista(1, lista); print(lista); end
; var a = crear-lista(5, vacio); var b = 42; print(lista?(a)); print(lista?(b)); end

; var lista = crear-lista("A", crear-lista("B", vacio)); print(cabeza(lista)); end
; var lista = crear-lista(1, crear-lista(2, crear-lista (3, vacio))); print(cola(lista)); end
; var a = crear-lista(1, crear-lista(2, vacio)); var b = crear-lista(3, crear-lista(4, vacio)); var c = append(a, b); print(c); end
; var l = crear-lista("a", crear-lista("b", crear-lista("c", vacio))); print(ref-list(l, 1)); end
; var l = crear-lista("a", crear-lista("b", crear-lista("c", vacio))); print(ref-list(l, 1)); print(ref-list(l, 10)); end

; var l = crear-lista(1, crear-lista(2, crear-lista(3, vacio))); set l = set-list(l, 1, 99); print(l); end

; var valor = 1.0; while (valor < 4.0) do set valor = (valor + 0.1); done print(valor); end
; var miLista = list(5, 6, 2, 3); for i in miLista do print(i); done end

; var contador = 0; while (contador < 3) do set contador = (contador + 1); done print(contador); end
; var x = 3; while (x > 0) do begin print(x); ; set x = (x - 1); end done print("Despegue"); end
; var datos = list("MathFlow", true, 42, vacio); for item in datos do print(item); done end
; var numeros = list(10, 20, 30); var suma = 0; for n in numeros do set suma = (suma + n); done print(suma); end


; crear-diccionario() end
; crear-diccionario("nombre": "Juanita", "edad": 20) end
; diccionario?(crear-diccionario()) end
; diccionario?(5) end
; var d = crear-diccionario("nombre": "Juanita"); diccionario?(d) end
; var d = crear-diccionario("nombre": "Juanita", "edad": 20); ref-diccionario(d, "nombre") end
; var d = crear-diccionario("nombre": "Juanita"); ref-diccionario(d, "edad") end
; var d = crear-diccionario("edad": 20); ref-diccionario(d, "edad") end
; var d = list(1,2,3); ref-diccionario(d, "edad") end
; var d = crear-diccionario(); set-diccionario(d, "nombre", "Juan") end
; var d = crear-diccionario("edad": 20); set-diccionario(d, "edad", 21) ref-diccionario(d, "edad") end
; var d = crear-diccionario("nombre": "Juanita", "edad": 20); claves(d) end
; var d = crear-diccionario("nombre": "Juanita", "edad": 20); valores(d) end

;funciones
#| 
func: crea funciones anónimas que pueden guardarse en variables.
def: declara funciones con nombre, incluyendo funciones recursivas.
[expFuncion (args...)]: aplica cualquier expresión que evalúe a una función. 
|#
; def sumar(a,b) { return (a + b); } [sumar (10 20)] end
; var doble = func(x) { return (x * 2); }; [doble (5)] end
; def saludar(nombre) { print(nombre); } [saludar ("Ana")] end
#|
def factorial(n) {
  if (n <= 1) then
    return 1;
  else
    return (n * [factorial ((n - 1))]);
}

[factorial (5)]
end
|#
#|
var apply = func(funcion) {
return [funcion (5)];
        };
var doble = func(x) {
            return (x*x);
              };
[apply (doble)];
end
|#

;por valor o referencia
#| var x = 5;
def cambiar(a) {
  set a = 99;
  return a;
}

[cambiar (x)]
x
end
|#
#|
var l = list(1,2,3);

def cambiarLista(lst) {
  set-list(lst, 1, 99);
}

[cambiarLista (l)]
ref-list(l, 1)
end
|#
#|
var doble = func(x) {
  return (x * 2);
};

def aplicar(f, valor) {
  return [f (valor)];
}

[aplicar (doble 8)]
end
|#

;; Expresiones algebraicas

#|
symbol x;
symbol z;
print(x + z);
end
|#
#|
symbol x;
symbol y;
var z = 5;
var c = (x+ (y+3));
var d = (x * (y + z));
var e = (x - (2 + z));
print(c);
print(d);
print(e);
end
|#
