#lang racket

;: 1.
;; invert:
;; Proposito:
;; List -> List’ :Procedimiento que recibe como parametro una lista que se compone de pares que a su vez son listas de tamaño 2
;; y se retorna una lista similar a la inicial con los pares invertidos 
;;
;;<lista> := ()
;; := (<valor-de-scheme> <lista>)
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

;: 2.
;;down:
;; Proposito:
;; List -> List’ :Procedimiento que convierte la lista en una lista donde cada elemento es asociado a un nivel más
;; de parentesis comparado con su estado original 
;;
;;<lista> := ()
;; := (<valor-de-scheme> <lista>)
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

;: 3.
;; Función: list-set
;;
;; Proposito:modificar una lista dada (L) al reemplazar el elemento en la posición específica n (indexando desde cero)
;;con el elemento x proporcionado como argumento. La función devuelve una nueva lista que es idéntica a la lista original (L),
;;excepto que contiene el elemento x en la posición indicada.
;;
;; Parámetros:
;; - lista: representa la lista en la que deseas realizar la modificación.
;; - n: representa la posición (indexando desde cero) en la lista L donde deseas insertar el elemento x.  
;; - x: representa el elemento que deseas insertar en la posición
;;
;; Contrato:
;; (list-set lista n x)
;;  
;;   - Retorna una nueva lista con el elemento x en la posicion n
;;   - Si la lista está vacía, devuelve una lista vacía (empty).
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

;: 4.
;; Función: filter-in
;;
;; Proposito: con los dos argumentos recibidos (un predicado P y una lista L) retornar una lista que
;; contiene los elementos que pertenecen a L y que satisfacen el predicado P.
;;
;; Parámetros:
;; - lista: representa la lista en la que deseas realizar la modificación.
;; - P: predicado con el que se realizara el filtro.
;; 
;;
;; Contrato:
;; (filter-in P lista)
;;  
;;   - Retorna una nueva lista que contiene los elementos que pertenecen a L y que satisfacen el predicado P
;;   - Si la lista está vacía, devuelve una lista vacía (empty).
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

;: 6.
;; Función: swapper
;; Proposito: Recibe una lista de elementos, y dos elementos, realiza el recorrido de toda la lista, retorna una nueva lista similar modificada, poniendo e2 donde encuentra e1 y viceversa.
;;
;; Entradas:
;; e1: elemento 1
;; e2: elemento 2
;; l: lista de elementos
;;
;; Parámetros:
;; e1 : representa el elemento 1
;; e2 : representa el elemento 2
;; l  : representa la lista de elementos que se recorrera para generar la nueva lista con las modificaciones respectivas donde se encuentre e1 y e2
;;
;; Contrato:
;; (swapper e1 e2 l)
;;  
;;   - Valida si la lista es vacia como caso base.
;;   - Valida si el primer elemento de la lista es igual a e1, de ser asi construye una nueva lista con e2 en la cabeza y sigue contruyendo recursivamente el resto.
;;   - Valida si el primer elemento de la lista es igual a e2, de ser asi construye una nueva lista con e1 en la cabeza y sigue contruyendo recursivamente el resto.
;;   - Si ninguna validacion anterior se cumple, se construye la lista con la cabeza de l y se sigue contruyendo recursivamente con el resto de la lista.
;; Ejemplo de uso:
;; > (swapper 'x 'y '(y y x y x y x x y)))
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
;;--------------------------------------------------------------------
;; 9.
(define inversions
 (lambda (lista)
   (cond
     [(empty? lista) 0]
     [else (+ (aux-inversions (cdr lista) (car lista)) (inversions (cdr lista)) )]
     )
   )
 )

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
;;--------------------------------------------------------------------
;; 12.
(define filter-acum
  (lambda (a b f acum filter)
    (cond
      [(> a b) acum]
      [(filter a) (filter-acum (+ a 1) b f (f acum a) filter)]
      [else (filter-acum (+ a 1) b f acum filter)]
      )
    )
  )

;;---------------------------------------------------------------------
;;15.

(define count-odd-and-even
  (lambda (arbol)
    (cond
      [(empty? arbol) '(0 0)]
      [else (list (count-aux arbol even?) (count-aux arbol odd?))]
      )
    ))

(define count-aux
  (lambda (a f)
    (cond
      [(empty? a) 0]
      [(f (car a)) (+ 1 (count-aux (cadr a) f) (count-aux (caddr a) f))]
      [else (+ 0 (count-aux (cadr a) f) (count-aux (caddr a) f))]
      )))  

;;---------------------------------------------------------------------
;;18.


(define agregar-inicio
  (lambda (lista x)
    (cons x lista)
      )
  )

(define agregar-final
  (lambda (lista x)
    (cond
      [(empty? lista) (cons x empty)]
      [else (cons (car lista) (agregar-final (cdr lista) x))] 
      )
    ))

(define suma-listas
  (lambda (l1 l2)
    (cond
      [(empty? l1) empty]
      [else (cons (+ (car l1) (car l2)) (suma-listas (cdr l1) (cdr l2)) )]
      )
    ))

(define pascal
  (lambda (n)
    (aux-pascal n 1 '(1))
    ))

(define aux-pascal
  (lambda (n step lista)
    (cond
      [(= step n) lista]
      [else (aux-pascal n (+ step 1) (suma-listas (agregar-inicio lista 0) (agregar-final lista 0) ))
       ]
      )
    )
  )

