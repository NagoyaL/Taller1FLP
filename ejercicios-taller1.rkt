#lang racket

;: 1.
;; invert:
;; Proposito:
;; List -> List’ :Procedimiento que recibe como parametro una lista que se compone de pares que a su vez son listas de tamaño 2
;; y se retorna una lista similar a la inicial con los pares invertidos 
;;
;;<lista> := ()
;;        := (<lista-de-pares> <lista>)
;; Ejemplo de uso:
;; (invert '((1 2) (3 4) (5 6))) => '((2 1) (4 3) (6 5))


(define invert
  (lambda(lista)
    (if (empty? lista)
        '()
        (cons
          (list (cadr (car lista)) (caar lista))
          (invert (rest lista))
          )
     
      )
  ))

;;Pruebas
(invert '((1 2) (3 4) (5 6)))
(invert '((6 7) (1 4) (0 7)))

;: 2.
;;down:
;; Proposito:
;; List -> List’ :Procedimiento que convierte la lista en una lista donde cada elemento es asociado a un nivel más
;; de parentesis comparado con su estado original 
;;
;;<lista>      := ()
;;<lista>      := (<lista-exp> <lista>)
;;<lista-exp>  := <lista-valores-scheme>
;;
;; Ejemplo de uso:
;; (down ’(1 2 3)) => '((1) (2) (3))

(define down
  (lambda (lista)
    (if (empty? lista) empty
        (cons (list (car lista)) (down (cdr lista)))
        )
      )
  )
(down '(1 2 3))
(down '(un (objeto (mas)) complicado))


;: 3.
;; list-set
;;
;; Proposito:modificar una lista dada (L) al reemplazar el elemento en la posición específica n (indexando desde cero)
;;con el elemento x proporcionado como argumento. La función devuelve una nueva lista que es idéntica a la lista original (L),
;;excepto que contiene el elemento x en la posición indicada.
;;
;;<lista>      := ()
;;<lista>      := <lista-exp> <lista>
;;<lista-exp>  := <valores-scheme>
;;<lista-exp>  := <lista-valores-scheme>
;;
;;
;; Ejemplo de uso:
;; > (list-set ’(a b c d) 2 => ’(1 2))(a b (1 2) d)


(define list-set
  (lambda (lista n x)
  (cond
    [(or (empty? lista) (< n 0)) lista]
    [(zero? n) (cons x (cdr lista))]
    [else (cons (car lista) (list-set (cdr lista) (- n 1) x)) ]
      )
  ))

;;Pruebas:
(list-set '(a b c d) 2 'c)
(list-set '(a b c d) 3 '(1 5 10))

;: 4.
;; filter-in
;;
;; Proposito: 
;; P x L -> L’: con los dos argumentos recibidos (un predicado P y una lista L) retornar una lista que
;; contiene los elementos que pertenecen a L y que satisfacen el predicado P.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
;;
;; Ejemplo de uso:
;; > (filter-in number? '(a 2 (1 3) b 7))


(define filter-in
  (lambda (P L)
    (if (null? L)
        '()
        (if (P (car L))
            (cons (car L) (filter-in P (cdr L)))
            (filter-in P (cdr L))))))


;;Ejemplos:
> (filter-in number? '(7 4 1 3 b z))
> (filter-in number? '(a 2 (1 f) 2 7))


;;5.
;;Función: list-index
;; Propósito:
;;   Esta función recibe un predicado P y una lista L. Retorna (desde una posición inicial 0) el primer elemento de la lista
;;   que satisface el predicado P. Si no se encuentra ningún elemento que cumpla con el predicado, la función retorna #f.
;;
;; Signatura:
;;   (list-index P L)
;; 
;; Expresiones BNF:
;;   <lista> ::= ()
;;           |  (<valor-de-scheme> <lista>)
;;
;; Ejemplos de uso:
;;   (list-index number? '(a 2 (1 3) b 7)) => 1
;;   (list-index symbol? '(a (b c) 17 foo)) => 0
;;   (list-index symbol? '(1 2 (a b) 3)) => #f
;;
;; Requisitos:
;;   - El predicado P debe ser una función unaria que acepta un argumento y retorna un valor booleano.
;;   - La entrada L debe ser una lista que cumpla con la estructura definida por las expresiones BNF.
;;   - La función busca el primer elemento de la lista que satisface el predicado P y retorna su posición (indexando desde 0).
;;   - Si ningún elemento cumple con el predicado, la función retorna #f.
;;
;; Función auxiliar: list-index-helper
;; Propósito:
;;   Esta función auxiliar es utilizada por la función principal `list-index`. Recibe una lista `L` y un índice `index` como
;;   argumentos. Su propósito es buscar el primer elemento en la lista `L` que satisface el predicado `P` y retornar su
;;   posición (indexando desde 0). Si ningún elemento satisface el predicado, la función retorna `#f`.
;;
;; Signatura:
;;   (list-index-helper L index)
;; 
;; Expresiones BNF:
;;   <lista> ::= ()
;;           |  (<valor-de-scheme> <lista>)
;;
;; Ejemplo de uso:
;;   (list-index-helper '(a 2 (1 3) b 7) 0) => 1
;;
;; Requisitos:
;;   - La entrada `L` es una lista que cumple con la estructura definida por las expresiones BNF.
;;   - La entrada `P` es un predicado que acepta un argumento y devuelve un valor booleano.
;;   - La función busca el primer elemento en la lista `L` que satisface el predicado `P` y retorna su posición (indexando desde 0).
;;   - Si ningún elemento cumple con el predicado, la función retorna `#f`.
;;

(define list-index
  (lambda (P L)
    (letrec ((list-index-helper
              (lambda (L index)
                (cond
                  [(null? L) #f]
                  [(P (car L)) index]
                  [else (list-index-helper (cdr L) (+ index 1))]
                ))))
      (list-index-helper L 0)
    )
  ))

;: 6.
;; Función: swapper
;; Proposito: Recibe una lista de elementos, y dos elementos, realiza el recorrido de toda la lista, retorna una nueva lista similar modificada, poniendo e2 donde encuentra e1 y viceversa.
;;
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
;;
;; Ejemplo de uso:
;; > (swapper 'x 'y '(y y x y x y x x y))
;; > (swapper 'a 'd '(a d () c d))

(define swapper
  (lambda (e1 e2 l)
    (cond
      [(empty? l) empty]
      [(equal? (car l) e1) (cons e2 (swapper e1 e2 (cdr l)))]
      [(equal? (car l) e2) (cons e1 (swapper e1 e2 (cdr l)))]
      [else (cons (car l)  (swapper e1 e2 (cdr l)))]
      )
    )
  )
;;Pruebas
(swapper 'x 'y '(y y x y x y x x y))
(swapper 'a 'd '(a d () c d))

;: 7.

;; Función: help
;;
;; Proposito: 
;; L x L -> L’: Concatena dos listas, lst1 y lst2.
;;
;; <lista> := ()
;; := (<valor-de-scheme> <lista>)
;;
;; Ejemplo de uso:
;;  > (help '(1 2 3) '(4 5 6))

(define help
  (lambda(lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (help (cdr lst1) lst2)))))


;; Función: help2
;;
;; Propósito:
;; F x L -> L’: Aplica una función dada (func) a cada elemento de una lista (lst) y retorna
;; una nueva lista que contiene los resultados de aplicar func a cada elemento.
;;
;; <lista> := ()
;; := (<valor-de-scheme> <lista>)
;;
;; Ejemplo de uso:
;;
;; > (define (cuadrado x) (* x x))
;; > (help2 cuadrado '(1 2 3 4))

(define help2
  (lambda(func lst)
  (if (null? lst)
      '()
      (cons (func (car lst))
            (help2 func (cdr lst))))))




;; Función: cartesian-product
;;
;; Proposito: 
;; L x L -> L’: recibir como argumentos 2 listas de sımbolos sin repeticiones L1 y L2 y
;; retornar una lista de tuplas que representen el producto cartesiano entre L1
;; y L2. 
;;
;; <producto-cartesiano> := ()
;;                      := (<elemento-L1> <elemento-L2> <producto-cartesiano>)
;;
;; Ejemplo de uso:
;; > (cartesian-product '(a b c) '(x y))





(define cartesian-product
  (lambda(L1 L2)
  (if (null? L1)
      '()
      (help (help2 (lambda (x) (list (car L1) x)) L2)
                 (cartesian-product (cdr L1) L2)))))

;; Ejemplos:

  > (cartesian-product '(x y g) '(w j))
  > (cartesian-product '(s d y) '(2 1 9))



;;8.
;;Función: mapping
;; Propósito:
;;   Esta función recibe una función unaria F, y dos listas L1 y L2 de números. Retorna una lista de pares (a, b), donde a es un elemento de L1 y b es un elemento de L2, y se cumple la propiedad de que (F a) = b.
;;
;; Signatura:
;;   (mapping F L1 L2)
;; 
;; Expresiones BNF:
;;   <lista> ::= ()
;;           |  (<valor-de-scheme> <lista>)
;;
;; Ejemplos de uso:
;;   (mapping (lambda (d) (* d 2)) '(1 2 3) '(2 4 6)) => '((1 2) (2 4) (3 6))
;;   (mapping (lambda (d) (* d 3)) '(1 2 2) '(2 4 6)) => '((2 6))
;;   (mapping (lambda (d) (* d 2)) '(1 2 3) '(3 9 12)) => '()
;;
;; Requisitos:
;;   - La función recibe una función unaria F, que toma un argumento y devuelve un número.
;;   - L1 y L2 deben ser listas que cumplan con la estructura definida por las expresiones BNF.
;;   - La función genera pares (a, b) donde a es un elemento de L1 y b es un elemento de L2.
;;   - Se verifica que (F a) = b para cada par (a, b) en la lista resultante.
;;
;; Función auxiliar: helper
;; Propósito:
;;   Esta función auxiliar es utilizada por la función principal `mapping`. Recibe dos listas `L1` y `L2`, junto con una función
;;   unaria `F` que toma un elemento de `L1` y devuelve un valor. Su propósito es mapear los elementos de `L1` y `L2` de acuerdo
;;   con la función `F` y construir una lista de pares (a, b) donde `a` es un elemento de `L1` y `b` es un elemento de `L2`
;;   que cumple con la propiedad F(a) = b. Retorna la lista de pares resultante.
;;
;; Signatura:
;;   (helper L1 L2)
;; 
;; Expresiones BNF:
;;   <lista> ::= ()
;;           |  (<valor-de-scheme> <lista>)
;;
;; Ejemplo de uso:
;;   (helper '(1 2 3) '(2 4 6)) => '((1 2) (2 4) (3 6))
;;
;; Requisitos:
;;   - La entrada `L1` es una lista que cumple con la estructura definida por las expresiones BNF.
;;   - La entrada `L2` es una lista que cumple con la estructura definida por las expresiones BNF.
;;   - La entrada `F` es una función unaria que acepta un argumento y devuelve un valor.
;;   - La función mapea los elementos de `L1` y `L2` de acuerdo con la función `F`, construye una lista de pares y la retorna.
;;
(define mapping
  (lambda (F L1 L2)
    (define helper
      (lambda (L1 L2)
        (cond
          [(or (null? L1) (null? L2)) '()]
          [(= (F (car L1)) (car L2))
           (cons (list (car L1) (car L2))
                 (helper (cdr L1) (cdr L2)))]
          [else (helper (cdr L1) (cdr L2))]
          )
        )
      )
    (helper L1 L2)
    )
  )





;;--------------------------------------------------------------------
;; 9.
;; Función: inversions
;; Proposito: recibe como entradauna lista L, y determina el numero de inversiones de la lista
;;
;;
;;
;; <lista> := ()
;;         := (<numero> <lista>)
;;
;; Ejemplo de uso:
;; > (inversions ’(2 3 8 6 1))
;; >  (inversions ’(1 2 3 4))

(define inversions
 (lambda (lista)
   (cond
     [(empty? lista) 0]
     [else (+ (aux-inversions (cdr lista) (car lista)) (inversions (cdr lista)) )]
     )
   )
 )


;;aux-inversion
;;
;;Recibe el primer elemento de la lista y el resto, valida y cuenta las inversiones que tiene el elemento x en la lista.
;;
(define aux-inversions
  (lambda (lista x)
    (cond
      [(empty? lista) 0]
      [(if
        (> x (car lista))
        (+ 1 (aux-inversions (cdr lista) x))
        (+ 0 (aux-inversions (cdr lista) x))
        )]
      )
      )
  )

;;PRUEBAS
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))

;;--------------------------------------------------------------------


;: 10.

;; Función: aux
;;
;; Proposito: 
;; L x L -> L’: Esta función auxiliar toma dos listas, lst1 y lst2, y devuelve una nueva lista
;; que representa el producto cartesiano entre lst1 y lst2. Cada elemento de la lista
;; resultante es una tupla que contiene un elemento de lst1 y un elemento de lst2.
;;  
;;
;; <lista> := ()
;; := (<valor-de-scheme> <lista>)
;;
;; Ejemplo de uso:
;; > (up '((1 2) (3 4)))


(define aux
 (lambda (lst1 lst2)
  (cond
    ((null? lst1) lst2) ; Si la primera lista está vacía, devuelve la segunda lista.
    (else (cons (car lst1) (aux (cdr lst1) lst2))))))



;; Función: up
;;
;; Proposito: 
;; L -> L’: Recibe una lista L y devuelve una lista con los elementos de L y, si encuentra
;; sublistas en L, los elementos de esas sublistas también, eliminando un nivel de
;; anidación.
;;
;; <lista> := ()
;; := (<valor-de-scheme> <lista>)
;;
;; Ejemplo de uso:
;; > (up '((1 2) (3 4)))




(define up
  (lambda (L)
  (cond
    ((null? L) '()) ; Caso base: si la lista está vacía, devuelve una lista vacía.
    ((list? (car L)) ; Si el primer elemento es una lista, elimina un par de paréntesis y concatena el resultado.
     (aux (car L) (up (cdr L))))
    (else ; Si el primer elemento no es una lista, simplemente lo incluye en el resultado y continúa con el resto de la lista.
     (cons (car L) (up (cdr L)))))))

;;Ejemplo: 
> (up '((7 2) (3 2)))
> (up '((1 1) (2 4)))





;; 11.
;; Función: zip:
;; Propósito:
;;   Esta función recibe una función binaria F y dos listas L1 y L2 de igual tamaño. Retorna una lista donde la posición n-ésima corresponde al resultado de aplicar la función F sobre los elementos en la posición n-ésima en L1 y L2.
;;
;; Signatura:
;;   (zip F L1 L2)
;; 
;; Expresiones BNF:
;;   <lista> ::= ()
;;           |  (<valor-de-scheme> <lista>)
;;
;; Ejemplos de uso:
;;   (zip + '(1 4) '(6 2)) => '(7 6)
;;   (zip * '(11 5 6) '(10 9 8)) => '(110 45 48)
;;
;; Requisitos:
;;   - La función recibe una función binaria F que toma dos argumentos y devuelve un resultado.
;;   - L1 y L2 deben ser listas que cumplan con la estructura definida por las expresiones BNF.
;;   - Las listas L1 y L2 deben tener el mismo tamaño.
;;   - La función crea una lista donde la posición n-ésima contiene el resultado de aplicar F a los elementos correspondientes de L1 y L2.
;;

(define zip
  (lambda (F L1 L2)
    (if (null? L1)
        '()
        (cons (F (car L1) (car L2))
              (zip F (cdr L1) (cdr L2))))))


;;--------------------------------------------------------------------
;; 12.
;; Función: filter-acum
;; Proposito:  El procedimiento filter-acum aplicara la
;; funcion binaria F a todos los elementos que estan en el intervalo [a, b] y que
;; a su vez todos estos elementos cumplen con el predicado de la funcion filter,
;; el resultado se debe ir conservando en acum y debe retornarse el valor final de acum
;;
;; Ejemplo de uso:
;;> (filter-acum 1 10 + 0 odd?) => 25
;;> (filter-acum 1 10 + 0 even?) => 30



(define filter-acum
  (lambda (a b f acum filter)
    (cond
      [(> a b) acum]
      [(filter a) (filter-acum (+ a 1) b f (f acum a) filter)]
      [else (filter-acum (+ a 1) b f acum filter)]
      )
    )
  )
;;PRUEBAS:
(filter-acum 1 10 + 0 odd?)
(filter-acum 1 10 + 0 even?)

;;----------------------------------------------------------------------------------------------------------------------------------------

;: 13.

;; Función: apply-operation
;;
;; Proposito: 
;; F X N X N -> N’: Aplicar una operación matemática a dos números.
;;
;;<func-list> ::= ()
;;                | (<func> <func-list>)
;;
;;<num-list>  ::= ()
;;                | (<num> <num-list>)
;;
;; <func>      ::= + | - | * | /
;; 
;; Ejemplo de uso:
;; > (apply-operation '+ 5 3)



  (define apply-operation
    (lambda (func num1 num2)
    (cond
      ((eq? func '+) (+ num1 num2))
      ((eq? func '-) (- num1 num2))
      ((eq? func '*) (* num1 num2))
      ((eq? func '/) (/ num1 num2))
      (else (error "Operación no válida")))))
  
  (apply-operations funcs nums)))


;; Función: operate
;;
;; Proposito: 
;; L X L -> N’: Realizar operaciones matemáticas en pares de números utilizando operadores
;; especificados en una lista y números en otra lista.
;;
;; <lista> := ()
;; := (<valor-de-scheme> <lista>)
;;  
;; 
;; 
;; 
;; Ejemplo de uso:
;; > (operate '(+ * + - *) '(1 2 8 4 11 6))


(define operate
  (lambda (funcs nums)
  (define apply-operations
    (lambda (funcs nums)
      (if (null? funcs)
          nums
          (apply-operations (cdr funcs) (cons (apply-operation (car funcs) (car nums) (cadr nums)) (cddr nums))))))


;; Ejemplos: 
> (operate '(+ - + - *) '(1 2 6 4 2 2))
> (operate '(+ + + - *) '(1 2 5 4 1 6))




;;---------------------------------------------------------------------
;;14.
;;Función: path
;; Propósito:
;;   Esta función recibe un número entero n y un árbol binario de búsqueda (BST) representado como una lista. Retorna una lista que representa la ruta desde el nodo raíz del árbol hasta el nodo que contiene el número n. La ruta se indica por cadenas "left" y "right".
;;
;; Signatura:
;;   (path n BST)
;; 
;; Expresiones BNF:
;;   <árbol-binario> ::= ()
;;                   |  (<número-entero> <árbol-binario> <árbol-binario>)
;;
;; Ejemplos de uso:
;;   (path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ())))) => '(right left left)
;;   (path 5 '(10 (5 () ()) (15 (12 () ()) (20 () ())))) => '(left)
;;
;; Requisitos:
;;   - La función recibe un número entero n y un árbol binario de búsqueda (BST) que cumple con la estructura definida por las expresiones BNF.
;;   - La función busca la ruta desde el nodo raíz del árbol hasta el nodo que contiene el número n.
;;   - La ruta se representa como una lista de cadenas "left" y "right".
;;   - Si el número n es encontrado en el nodo raíz, el procedimiento retorna una lista vacía ().
;;
;; Función auxiliar: path-helper
;; Propósito:
;;   Esta función auxiliar es utilizada por la función principal `path`. Recibe dos argumentos: `current-path` y `current-node`,
;;   que representan el camino actual y el nodo actual en un árbol binario de búsqueda (BST). Su propósito es encontrar el camino
;;   desde la raíz del árbol hasta el nodo con el valor `n` en el BST y representarlo como una lista de direcciones ('left' o 'right').
;;   Retorna el camino como una lista de direcciones o una lista vacía si el valor `n` no se encuentra en el árbol.
;;
;; Signatura:
;;   (path-helper current-path current-node)
;; 
;; Expresiones BNF:
;;   <nodo> ::= ()
;;          |  (<valor-de-scheme> <nodo>)
;;
;; Ejemplo de uso:
;;   (path-helper '() '(10 (5 () ()) (15 () (20 () ())))) => '("left" "right")
;;
;; Requisitos:
;;   - La entrada `current-path` es una lista que representa el camino actual desde la raíz del árbol hasta el nodo actual.
;;   - La entrada `current-node` es un nodo del árbol binario de búsqueda.
;;   - La función busca el camino desde la raíz hasta el nodo con el valor `n` en el BST y lo representa como una lista de direcciones.
;;   - Si el valor `n` no se encuentra en el árbol, la función retorna una lista vacía.
;; Función auxiliar: multiplicar-matriz
;; Propósito:
;;   Esta función auxiliar es utilizada por la función principal `prod-scalar-matriz`. Recibe una matriz representada como una
;;   lista de listas y un vector representado como una lista. Su propósito es multiplicar la matriz por el vector dado y
;;   retornar el resultado como una nueva matriz.
;;
;; Signatura:
;;   (multiplicar-matriz matriz vector)
;; 
;; Expresiones BNF:
;;   <matriz> ::= ()
;;            |  (<lista> <matriz>)
;;   <lista>  ::= ()
;;            |  (<valor-de-scheme> <lista>)
;;
;; Requisitos:
;;   - La entrada `matriz` debe ser una matriz representada como una lista de listas que cumpla con la estructura definida por las
;;     expresiones BNF.
;;   - La entrada `vector` debe ser un vector representado como una lista.
;;   - La función realiza la multiplicación de la matriz por el vector, y retorna el resultado como una nueva matriz.
;;

(define path
  (lambda (n BST)
    (define path-helper
      (lambda (current-path current-node)
        (cond
          ((null? current-node) '())
          ((= n (car current-node)) current-path)
          ((< n (car current-node))
           (cons "left" (path-helper current-path (cadr current-node))))
          (else
           (cons "right" (path-helper current-path (caddr current-node))))
          )))
    (path-helper '() BST)
  )
)


;;---------------------------------------------------------------------
;;15.
;;count-odd-and-even
;;
;;Proposito: toma un  ́arbol binario y retorna una lista con dos elementos correspondientes
;; a la cantidad de pares e impares en arbol.
;;
;;<arbol> :=()
;;<arbol> :=<int> <arbol> <arbol>
;;
;;ejemplos:
;;(count-odd-and-even ’(14 (7 () (12 () ()))
;;(26 (20 (17 () ())
;;())
;;(31 () ())))) => (4 3)

(define count-odd-and-even
  (lambda (arbol)
    (cond
      [(empty? arbol) '(0 0)]
      [else (list (count-aux arbol even?) (count-aux arbol odd?))]
      )
    ))
;; count-aux
;; a f => int
;;proposito: recibe el arbol y el predicado de par e impar, con el fin de retornar el total de elementos que cumplen con el predicado
(define count-aux
  (lambda (a f)
    (cond
      [(empty? a) 0]
      [(f (car a)) (+ 1 (count-aux (cadr a) f) (count-aux (caddr a) f))]
      [else (+ 0 (count-aux (cadr a) f) (count-aux (caddr a) f))]
      )))  
;;pruebas
(count-odd-and-even '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ()))))

(count-odd-and-even '(5
                      (4 (3 () ())())
                      (3 (1 () ())())))
;;---------------------------------------------------------------------

16.

;; Función: operacion
;;
;; Proposito: 
;; F X N X N -> N’: Realiza una operación matemática entre dos números según la función especificada.
;;
;; <lista> := ()
;; := (<valor-de-scheme> <lista>)
;;  
;; 
;; Ejemplo de uso:
;; > (apply-operation '+ 3 5)




(define operacion
  (lambda (a operador b)
    (cond
      [(equal? operador 'suma) (+ a b)]
      [(equal? operador 'resta) (- a b)]
      [(equal? operador 'multiplica) (* a b)]
      )
    ))


;; Función: Operar-binarias
;;
;; Proposito: 
;; N X F X N -> N’: recibe como parametro una operacion binaria valida y retorna el resultado de hacer
;; las operaciones suma, resta y multiplicacion correspondientes
;;
;; <lista> := ()
;; := (<valor-de-scheme> <lista>)
;;  
;; 
;; Ejemplo de uso:
;; > (Operar-binarias '(5 suma (7 resta 2)))


(define Operar-binarias
  (lambda (x)
    (cond
      [(number? x) x]
      [(list? x) (operacion (Operar-binarias (car x)) (cadr x) (Operar-binarias (caddr x)))]
  )))


;; Pruebas 
> (Operar-binarias '(5 suma (7 resta 2)))
> (Operar-binarias '(2 multiplica (7 suma 16)))




;;17.
;;Función: prod-scalar-matriz
;; Propósito:
;;   Esta función recibe una matriz representada como una lista de listas y un vector representado como una lista. Retorna el resultado de realizar la multiplicación matriz por vector.
;;
;; Signatura:
;;   (prod-scalar-matriz matriz vector)
;; 
;; Expresiones BNF:
;;   <matriz> ::= ()
;;            |  (<lista> <matriz>)
;;   <lista>  ::= ()
;;            |  (<valor-de-scheme> <lista>)
;;
;; Ejemplos de uso:
;;   (prod-scalar-matriz '((1 1) (2 2)) '(2 3)) => '((2 3) (4 6))
;;   (prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3)) => '((2 3) (4 6) (6 9))
;;
;; Requisitos:
;;   - La función recibe una matriz como una lista de listas y un vector como una lista.
;;   - La matriz y el vector deben cumplir con la estructura definida por las expresiones BNF.
;;   - La función realiza la multiplicación de la matriz por el vector.
;;
(define prod-scalar-matriz
  (lambda (matriz vector)
    (define producto-fila
      (lambda (fila vector)
        (if (null? fila)
            '()
            (cons (* (car fila) (car vector))
                  (producto-fila (cdr fila) (cdr vector))))))
    
    (define multiplicar-matriz
      (lambda (matriz vector)
        (cond
          ((null? matriz) '())
          (else
           (cons (producto-fila (car matriz) vector)
                 (multiplicar-matriz (cdr matriz) vector))))))

    (multiplicar-matriz matriz vector))
  )




;;---------------------------------------------------------------------
;;18.
;;pascal
;;
;;Proposito: Recibe un numero entero n, retorna la enesima fila del triangulo de pascal
;;
;;<lista> :=<int>
;;<lista> :=<int> <lista>
;;
;;ejemplos:
;;> (pascal 5) => (1 4 6 4 1)
;;> (pascal 1) => (1)
(define pascal
  (lambda (n)
    (aux-pascal n 1 '(1))
    ))

;;agregar-inicio
;;proposito: recibe una lista y un elemento x, retorna una lista con el elemento x en la cabeza de la lista.
(define agregar-inicio
  (lambda (lista x)
    (cons x lista)
      )
  )
;;agregar-final
;;proposito: recibe una lista y un elemento x, retorna una lista con el elemento x al final de la lista.
(define agregar-final
  (lambda (lista x)
    (cond
      [(empty? lista) (cons x empty)]
      [else (cons (car lista) (agregar-final (cdr lista) x))] 
      )
    ))
;;suma-listas
;;proposito: recibe dos listas y retorna una lista con la suma (l1 + l2) de acuerdo a su posicion correspondiente.
(define suma-listas
  (lambda (l1 l2)
    (cond
      [(empty? l1) empty]
      [else (cons (+ (car l1) (car l2)) (suma-listas (cdr l1) (cdr l2)) )]
      )
    ))


;;aux-pascal
;; proposito: recibe la lista, el n y el step, se encarga de recibir los datos inicializados desde la funcion pascal y realizar
;; la recursion de las sumas, con el fin de obtener la fila siguiente del triangulo de pascal.
;; n es el final de la recursion, step se encarga de contar la fila actual.
(define aux-pascal
  (lambda (n step lista)
    (cond
      [(= step n) lista]
      [else (aux-pascal n (+ step 1) (suma-listas (agregar-inicio lista 0) (agregar-final lista 0) ))
       ]
      )
    )
  )


;;PRUEBAS PASCAL:
 (pascal 5)
 (pascal 1)
