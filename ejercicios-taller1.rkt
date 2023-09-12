#lang racket

;: 1.
;; Función: invert
;;
;; Proposito: Recibe como parametro una lista que se compone de pares que a su vez son listas de tamaño 2
;; y se retorna una lista similar a la iniciar con los pares invertidos 
;; Parámetros:
;; - lista: La lista de pares que se va a invertir.
;;
;; Contrato:
;; (invert lista)
;;   - lista: Lista de pares (listas de exactamente dos elementos).
;;   - Retorna una nueva lista que contiene pares con los elementos invertidos.
;;   - Si la lista está vacía, devuelve una lista vacía ('()).
;;
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
;; Función: down
;;
;; Proposito: Convierte la lista en una lista donde cada elemento es asociado a un nivel más
;; de parentesis comparado con su estado original
;;
;; Parámetros:
;; - lista:La lista de la cual se van a crear las sublistas.
;;
;;
;; Contrato:
;; (down lista)
;;   - lista: Lista de elementos.
;;   - Retorna una nueva lista de sublistas, donde cada sublista contiene un solo elemento de la lista original.
;;   - Si la lista está vacía, devuelve una lista vacía (empty).
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
;; Función: list-index
;;
;; Proposito:funcion llamada list-index que debe recibir dos
;; argumentos: un predicado P y una lista L. La funcion retorna el primer elemento de la lista que satisface el predicado L
;;
;; Parámetros:
;;
;; - lista:La lista de la cual se van a crear las sublistas.
;; 
;; Ejemplo de uso:
;; > (display (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))) ;
;; Output: ("univalle" "racket" "flp")




