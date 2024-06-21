#lang eopl

; Proyecto final - Fundamentos de interpretación y compilación de lenguajes de programacion
; Autores: Santiago Villa Salazar (2259527-3743), Camilo Valencia Romero (2259497-3743), Edgar Fabian Rueda Colonia (2259606-3743)
; Profesor: Carlos Andres Delgado

;******************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variable

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expresion>
;;                      <a-program (exp)>
;;  <expresion>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expresion>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expresion>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {<identificador> = <expresion>}* in <expresion>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expresion>
;;                      <proc-exp (ids body)>
;;                  ::= (<expresion> {<expresion>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identificador ({identificador}(,)) = <expresion>} in <expresion>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expresion> {; <expresion>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identificador> = <expresion>
;;                     <set-exp (id rhsexp)>
;;  <primitive>     ::= + | - | * | add1 | sub1

;******************************

;******************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?"))) symbol)
  (digitoDecimal
   (digit (arbno digit)) number)
  (digitoDecimal
   ("-" digit (arbno digit)) number)
  (digitoBinario
   ("b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoBinario
   ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoOctal
   ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoOctal
   ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoHexadecimal
   ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (digitoHexadecimal
   ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (flotante
   (digit (arbno digit) "." digit (arbno digit)) number)
  (flotante
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expresion) a-program)

    ;; Expresiones numéricas
    (expresion (numero-exp) num-exp)
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) flotante-num)

    ;; Expresiones aritméticas
    (expresion ("(" expresion primitive expresion ")") prim-num-exp)
    ;; Primitivas numéricas
    (primitive ("+") sum-prim)
    (primitive ("-") minus-prim)
    (primitive ("*") mult-prim)
    (primitive ("mod") mod-prim)
    (primitive ("pow") elevar-prim)
    (primitive ("<") menor-prim)
    (primitive (">") mayor-prim)
    (primitive ("<=") menorigual-prim)
    (primitive (">=") mayorigual-prim)
    (primitive ("!=") diferente-prim)
    (primitive ("==") igual-prim)


    ;; Identificadores y cadenas
    (expresion (identificador) var-exp)
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)

    ;; Listas
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)
    ;; Primitivas listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ;; Condicionales
    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)

    ;; Funciones y llamadas
    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    (expresion ("call" expresion "(" (separated-list expresion ",") ")") app-exp)


    ;; Ligaduras modificables
    (expresion ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (expresion ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)

    ;; Bloques de expresiones
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;; Estructuras
    ;(struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)
    ;; Instanciación y uso de estructuras
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    (expresion ("get" expresion "." identificador) get-struct-exp)
    (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ;; Primitivas de cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)

    ;; Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)

    ;; Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;; Arrays y primitivas de arrays
    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)

    ;; Reconocimiento de patrones
    (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)
    ;; Expresiones regulares
    (regular-exp (identificador "::" identificador) list-match-exp)
    (regular-exp ("numero" "(" identificador ")") num-match-exp)
    (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)

    ;; Primitivas booleanas
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)

    ;; Expresión literal
    ;;(expresion (number) lit-exp)
    ))

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************
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

;*******************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expresion body (empty-env))))))


;eval-expresion: <expresion> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (num-exp (tipo_numero)
        (cases numero-exp tipo_numero
          (decimal-num (dato)  dato)
          (bin-num (dato)  dato)
          (octal-num (dato) dato)
          (hex-num (dato) dato)
          (flotante-num (dato) dato)
        )
      )
      (prim-bool-exp (prim args)
        (evaluate-boolean prim (eval-rands args env))
      )
      (var-exp (id) (apply-env env id))
      (true-exp () #T)
      (false-exp () #F)
      (if-exp (test-exp true-exp false-exp)
        (if (eval-expresion test-exp env)
            (eval-expresion true-exp env)
            (eval-expresion false-exp env)
        )
      )
      (func-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args env)
                     (eopl:error 'eval-expresion
                                 "Attempt to apply non-procedure ~s" proc))))
      (set-exp (id valor_nuevo)
        (let ((valor_nuevo_evaluado (eval-expresion valor_nuevo env)))
          (modificar-ligadura env id valor_nuevo_evaluado)
          'void)
        )
      (prim-num-exp (exp1 prim exp2)
        (let ((eexp1 (eval-expresion exp1 env))
            (eexp2 (eval-expresion exp2 env))
          )
          (apply-primitive prim eexp1 eexp2)))
      (prim-cad-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive_string prim args)
        )
      )
      (begin-exp (exp exps)
                 (let loop ((acc (eval-expresion exp env))
                             (exps exps))
                    (if (null? exps)
                        acc
                        (loop (eval-expresion (car exps)
                                               env)
                              (cdr exps)))))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expresion body
                                  (extend-env ids args env))))
      (lvar-exp (ids rands body)
        (let ((args (eval-rands rands (extend-modificable-env ids (list->vector rands) env))))
                 (eval-expresion body
                                  (extend-modificable-env ids (list->vector args) env))))
      (cadena-exp (identificador Lidentifica)
        (letrec
          [
            (crear_string
              (lambda (lids)
                (cond
                  [(null? lids) ""]
                  [else (string-append " " (symbol->string (car lids)) (crear_string (cdr lids)))]
                )
              )
            )
          ]
          (string-append (symbol->string identificador) (crear_string Lidentifica))
        )
      )
      (lista-exp (Lexp)
        (eval-rands Lexp env))
      (cons-exp (exp1 exp2)
        (cons (eval-rand exp1 env) (eval-rand exp2 env)))
      (prim-list-exp (prim exp)
        (let ((arg (eval-rand exp env)))
          (apply-list prim arg)))
      (while-exp (boolean_exp body_exp)
          (cond
              [(eval-expresion boolean_exp env)
                (eval-expresion body_exp env)
                (eval-expresion exp env)
              ]
              [else 'void]
          )
      )
      (for-exp (variable start-exp end-exp sum-exp body-exp)
        (let ((start (eval-expresion start-exp env))
              (end (eval-expresion end-exp env))
              (sum (eval-expresion sum-exp env))
            )
          (for-estructura body-exp variable start sum end env)
        )
      )
      (switch-exp (variable_exp list_caso list_exp default_exp)
        (letrec ((valor (eval-expresion variable_exp env))
            (coinciden
              (lambda (caso list_e valor)
                (cond
                  [(null? caso) (eval-expresion default_exp env)]
                  [(equal? valor (eval-expresion (car caso) env)) (eval-expresion (car list_e) env)]
                  [else (coinciden (cdr caso) (cdr list_e) valor)]
                )
              )
            )
          )
          (coinciden list_caso list_exp valor)
        )
      )
      (array-exp (lista)
        (list->vector (eval-rands lista env))
      )
      (prim-array-exp (primitiva lista_argumentos)
        (primitiva-array primitiva (eval-rands lista_argumentos env))
      )
      (empty-list-exp () '())
      (match-exp (exp_variable list_casos lista_exp)
        (let ((valor (eval-expresion exp_variable env)))
          (match-valor valor list_casos lista_exp env)
        )
      )
      (new-struct-exp (identi lista_atributos)
        0
      )
      (get-struct-exp (struc atributo)
        0
      )
      (set-struct-exp (struc_var atributo nuevo_valor)
        0
      )
    )
  )
)


; funciones auxiliares para aplicar eval-expresion a cada elemento de una
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expresion rand env)))

; Funciones para operaciones booleanas
(define and-f
  (lambda (a b)
    (and a b)))

(define or-f
  (lambda (a b)
    (or a b)))

(define xor-f
  (lambda (a b)
    (or (and a (not b)) (and (not a) b))))

(define evaluate-list
  (lambda (func_operadora list)
    (cond
      [(null? (cdr list)) (car list)]
      [else (func_operadora (car list) (evaluate-list func_operadora (cdr list)))]
    )
  )
)

(define evaluate-boolean
  (lambda (prim args)
    (cases primitivaBooleana prim
      (or-prim () (evaluate-list or-f args))
      (xor-prim () (evaluate-list xor-f args))
      (and-prim () (evaluate-list and-f args))
      (not-prim () (not (car args)))
    )
  )
)

;; Métodos para números

; Funciones auxiliares para manipulación de cadenas
(define eliminar-caracter
  (lambda (stri ch)
    (letrec
      [
        (remove-char
          (lambda (str ch)
            (cond
              [(null? str) '()]
              [else
              (if (char=? ch (car str))
                  (remove-char (cdr str) ch)
                  (cons (car str) (remove-char (cdr str) ch)))
              ]
            )
          )
        )
      ]
    (list->string (remove-char (string->list stri) ch))
    )
  )
)

(define reemplazar-caracter
  (lambda (stri ch new-ch)
    (letrec
      [
        (replace-caracter
          (lambda (str ch new-ch)
            (cond
              [(null? str) '()]
              [else
              (if (char=? ch (car str))
                  (cons new-ch (replace-caracter (cdr str) ch new-ch))
                  (cons (car str) (replace-caracter (cdr str) ch new-ch)))
              ]
            )
          )
        )
      ]
      (list->string (replace-caracter (string->list stri) ch new-ch))
    )
  )
)



; Aplicación de operaciones según la base del número
(define aplicar-segun-base
  (lambda (operacion arg1 arg2 transformar)
    (cond
      [(string? arg1)
       (cond
         [(or (equal? (string-ref arg1 0) #\b)
              (and (equal? (string-ref arg1 1) #\b)
                   (equal? (string-ref arg1 0) #\-)))
          (transformar
            (operacion (string->number (eliminar-caracter arg1 #\b) 2) (string->number (eliminar-caracter arg2 #\b) 2))
            2)
         ]
         [(or (equal? (string-ref arg1 0) #\h) (and (equal? (string-ref arg1 1) #\h) (equal? (string-ref arg1 0) #\-)))
          (transformar
            (operacion (string->number (reemplazar-caracter arg1 #\h #\#) 16) (string->number (reemplazar-caracter arg2 #\h #\#) 16))
            16)
         ]
         [(or (equal? (string-ref arg1 1) #\x)
              (and (equal? (string-ref arg1 2) #\x)
                   (equal? (string-ref arg1 0) #\-)))
          (transformar
            (operacion (string->number (eliminar-caracter arg1 #\x) 8) (string->number (eliminar-caracter arg2 #\x) 8))
            8)
         ]
         [else (eopl:error "ingreso un numero invalido")]
       )
      ]
      [else (operacion arg1 arg2)]
    )
  )
)

(define retorno_boleano
  (lambda (out base) out)
)

; Conversión de número a cadena según la base
(define convertir-numero
  (lambda (num base)
    (let* ((es-negativo? (< num 0))
           (abs-num (abs num)))
      (cond
        [(equal? base 2) (if es-negativo?
                 (string-append "-" "b" (number->string abs-num 2))
                 (string-append "b" (number->string abs-num 2)))]
        [(equal? base 8) (if es-negativo?
                 (string-append "-" "0x" (number->string abs-num 8))
                 (string-append "0x" (number->string abs-num 8)))]
        [(equal? base 16) (if es-negativo?
                  (string-append "-" "hx" (number->string abs-num 16))
                  (string-append "hx" (number->string abs-num 16)))]
        [else (eopl:error "Base no soportada")]
      )
    )
  )
)

(define apply-primitive
  (lambda (prim arg1 arg2)
    (cases primitive prim
      (sum-prim () (aplicar-segun-base + arg1 arg2 convertir-numero ))
      (minus-prim () (aplicar-segun-base - arg1 arg2 convertir-numero))
      (mult-prim () (aplicar-segun-base * arg1 arg2 convertir-numero))
      (mayor-prim () (aplicar-segun-base > arg1 arg2 retorno_boleano))
      (menor-prim () (aplicar-segun-base < arg1 arg2 retorno_boleano))
      (menorigual-prim () (aplicar-segun-base <= arg1 arg2 retorno_boleano))
      (mayorigual-prim () (aplicar-segun-base >= arg1 arg2 retorno_boleano))
      (diferente-prim ()( not (equal? arg1 arg2)))
      (igual-prim () (equal? arg1 arg2))
      (mod-prim () (aplicar-segun-base remainder arg1 arg2 convertir-numero))
      (elevar-prim () (aplicar-segun-base expt arg1 arg2 convertir-numero))
    )
  )
)


;apply-primitive: <primitiva> <list-of-expresion> -> numero
(define apply-primitive_string
  (lambda (prim args)
    (cases primitivaCadena prim
      (length-primCad () (string-length (car args)))
      (index-primCad ()  (string  (string-ref (car args) (cadr args))))
      (concat-primCad ()
        (letrec
          [
            (concat
              (lambda (list_stri)
                (cond
                  [(null? list_stri) ""]
                  [else (string-append (car list_stri ) (concat (cdr list_stri)))]
                )
              )
            )
          ]
          (concat args)
        )
      )
      )))

;metodos de listas
; aplicar-primitiva lista
(define apply-list
  (lambda (prim arg)
    (cases primitivaListas prim
      (first-primList () (car arg))
      (rest-primList () (cdr arg))
      (empty-primList () (null? arg))
    )
  )
)

;metodos de vectores
(define subvector
  (lambda (vect posicion_inicial posicion_final)
    (cond
      [(= posicion_inicial posicion_final) (cons (vector-ref vect posicion_inicial) '())]
      [else (cons (vector-ref vect posicion_inicial) (subvector vect (+ posicion_inicial 1) posicion_final))]
    )
  )
)

(define primitiva-array
  (lambda (prim arg)
    (cases primitivaArray prim
      (length-primArr () (vector-length (car arg)))
      (slice-primArr () (list->vector (subvector  (car arg) (cadr arg) (caddr arg))))
      (index-primArr () (vector-ref (car arg) (cadr arg)))
      (setlist-primArr ()
        (vector-set! (car arg) (cadr arg) (caddr arg))
        (car arg)
      )
    )
  )
)
;estructuras
;for
(define for-estructura
  (lambda (body-exp variable indice sum end env)
    (cond
      [(< indice end)
        (eval-expresion body-exp (extend-env (list variable) (list indice) env)); se crea un ambiente temporal con el valor de el indice en ese momento
        (for-estructura body-exp variable (+ indice sum) sum end env)
      ]
      [else 'void]
    )
  )
)
;entra una lista de variables pero si esta no es suficiente, la ultima toma el valor del resto del vector
(define asignar_vector_a_variables
  (lambda (lis_ids vect acc)
    (cond
      [(null? (cdr lis_ids)) (cons (subvector vect acc (- (vector-length vect) 1)) '())]
      [else (cons (vector-ref vect acc) (asignar_vector_a_variables (cdr lis_ids) vect (+ acc 1)))]
    )
  )
)

;match
(define match-valor
  (lambda (valor primt_match expresi_match env)
    (cases regular-exp (car primt_match)
      (empty-match-exp ()
        (cond
          [(null? valor)
           (eval-expresion (car expresi_match) env)]
          [else
           (match-valor valor (cdr primt_match) (cdr expresi_match) env)]
        )
      )
      (list-match-exp (cabeza cola)
        (cond
          [(list? valor)
           (eval-expresion (car expresi_match)
                           (extend-env (cons cabeza (cons cola '()))
                                       (list (car valor) (cdr valor)) env))]
          [else
           (match-valor valor (cdr primt_match) (cdr expresi_match) env)]
        )
      )
      (num-match-exp (ids)
        (cond
          [(number? valor)
           (eval-expresion (car expresi_match)
                           (extend-env (list ids) (list valor) env))]
          [(or (equal? (string-ref valor 0) #\b)
               (and (equal? (string-ref valor 1) #\b)
                    (equal? (string-ref valor 0) #\-)))
           (eval-expresion (car expresi_match)
                           (extend-env (list ids) (list valor) env))]
          [(or (equal? (string-ref valor 0) #\h)
               (and (equal? (string-ref valor 1) #\h)
                    (equal? (string-ref valor 0) #\-)))
           (eval-expresion (car expresi_match)
                           (extend-env (list ids) (list valor) env))]
          [(or (equal? (string-ref valor 1) #\x)
               (and (equal? (string-ref valor 2) #\x)
                    (equal? (string-ref valor 0) #\-)))
           (eval-expresion (car expresi_match)
                           (extend-env (list ids) (list valor) env))]
          [else
           (match-valor valor (cdr primt_match) (cdr expresi_match) env)]
        )
      )
      (cad-match-exp (ids)
        (cond
          ;; Detectar binario: b o -b seguido de 0 o 1
          [(or (and (equal? (string-ref valor 0) #\b)
                    (or (equal? (string-ref valor 1) #\0)
                        (equal? (string-ref valor 1) #\1)))
              (and (equal? (string-ref valor 0) #\-)
                    (equal? (string-ref valor 1) #\b)
                    (or (equal? (string-ref valor 2) #\0)
                        (equal? (string-ref valor 2) #\1))))
          (match-valor valor (cdr primt_match) (cdr expresi_match) env)]

          ;; Detectar hexadecimal: hx o -hx
          [(or (and (equal? (string-ref valor 0) #\h)
                    (equal? (string-ref valor 1) #\x))
              (and (equal? (string-ref valor 0) #\-)
                    (equal? (string-ref valor 1) #\h)
                    (equal? (string-ref valor 2) #\x)))
          (match-valor valor (cdr primt_match) (cdr expresi_match) env)]

          ;; Detectar octal: 0x o -0x
          [(or (and (equal? (string-ref valor 0) #\0)
                    (equal? (string-ref valor 1) #\x))
              (and (equal? (string-ref valor 0) #\-)
                    (equal? (string-ref valor 1) #\0)
                    (equal? (string-ref valor 2) #\x)))
          (match-valor valor (cdr primt_match) (cdr expresi_match) env)]

          ;; Evaluar la expresión si no cumple ninguna de las anteriores
          [else
          (eval-expresion (car expresi_match)
                          (extend-env (list ids) (list valor) env))]
        )
      )
      (bool-match-exp (ids)
        (cond
          [(boolean? valor)
           (eval-expresion (car expresi_match)
                           (extend-env (list ids) (list valor) env))]
          [else
           (match-valor valor (cdr primt_match) (cdr expresi_match) env)]
        )
      )
      (array-match-exp (ids)
        (cond
          [(vector? valor)
           (eval-expresion (car expresi_match)
                           (extend-env ids (asignar_vector_a_variables ids valor 0) env))]
          [else
           (match-valor valor (cdr primt_match) (cdr expresi_match) env)]
        )
      )
      (default-match-exp ()
        (eval-expresion (car expresi_match) env)
      )
    )
  )
)

;fin de estructuras

;*******************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))


;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args envI)
    (cases procval proc
      (closure (ids body env)
        (cases environment env
          (extend-modificable-env (lid lval next-env)
            (eval-expresion body (extend-env ids args envI))
          )
          (else (eval-expresion body (extend-env ids args envI)))
        )
      )
    )
  )
)

;*******************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (syms (list-of symbol?))
   (valores (list-of scheme-value?))
   (env environment?))
  (extend-modificable-env
   (syms (list-of symbol?))
   (valores vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))



;funcion para buscar una variable en un ambiente
(define buscador_no_modificable
  (lambda (lid lval valBus next-amb)
    (cond
      [(null? lid) (apply-env next-amb valBus)]
      [(equal? (car lid) valBus) (car lval)]
      [else (buscador_no_modificable (cdr lid) (cdr lval) valBus next-amb)]
    )
  )

)
(define buscador_modificable
  (lambda (lid lval valBus next-amb acc env )
    (cond
      [(null? lid) (apply-env next-amb valBus)]
      [(equal? (car lid) valBus)
        (if (expresion? (vector-ref lval acc))
          (eval-expresion (vector-ref lval acc) env)
          (vector-ref lval acc)
        )
      ]
      [else (buscador_modificable (cdr lid) lval valBus next-amb (+ acc 1) env)]
    )
  )
)

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env () (eopl:error "variable not found" sym))
      (extend-env (lid lval next-env)
        (buscador_no_modificable lid lval sym next-env)
      )
      (extend-modificable-env (lid lval next-env)
        (buscador_modificable lid lval sym next-env 0 env)
      )
    )
  )
)


(define buscador!
  (lambda (lid lval valBus next-amb)
    (cond
      [(null? lid) (extend-env (apply-env next-amb valBus))]
      [(equal? (car lid) valBus) (car lval)]
      [else (buscador_no_modificable (cdr lid) (cdr lval) valBus next-amb)]
    )
  )
)

(define modificar-ligadura
  (lambda (env sym val)
    (cases environment env
      (extend-modificable-env (symB Vect envNext)
        (letrec
          [
            (buscador_indice
              (lambda (Lsym acc)
                (cond
                  [(null? Lsym) (eopl:error "indice no encontrado " sym)]
                  [(equal? (car Lsym) sym) acc]
                  [else (buscador_indice (cdr Lsym) (+ acc 1))]
                )
              )
            )
          ]
          (vector-set! Vect (buscador_indice symB 0) val)
        )
      )
      (extend-env (symB list next-env)
        (modificar-ligadura next-env sym val)
      )
      (else (eopl:error "variable not found in var"))
    )
  )
)

;******************************


(show-the-datatypes)

;(define un-programa-dificil (a-program una-expresion-dificil))

;; Descomentar para pruebas, vuelva a comentar para envitar
(interpretador)

#|
No tocar
Exporar funciones
|#
(provide (all-defined-out))
