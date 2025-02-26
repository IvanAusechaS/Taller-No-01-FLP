#lang racket
;Ejercicio No 03
;list-set
(define list-set
  (lambda(L n x)
    (cond
      [(null? L) '()]
      [(= n 0) (cons x(cdr L))]
      [else (cons(car L)(list-set(cdr L)(- n 1)x))]
      ))
)
;Pruebas
;(list-set '(a b c d) 2 '(1 2))
;(list-set '(a b c d) 3 '(1 5 10))
;(list-set '((2 3) 3 (1 4)) 1 '(4 6))
;(list-set '((2 3) 3 (1 4)) 1 '(4 6 6 7 9 6))
;Ejercicio No 06
;swapper
(define swapper
  (lambda(E1 E2 L)
    (cond
      [(null? L) '()]
      [(eqv? (car L) E1) (cons E2 (swapper E1 E2(cdr L)))]
      [(eqv? (car L) E2) (cons E1 (swapper E1 E2(cdr L)))]
      [else (cons(car L)(swapper E1 E2 (cdr L)))]
      )
    )
  )
;Pruebas
;(swapper 'b 'c '(h b c a b c))
;(swapper 'a 'd '(a b c d))
;(swapper 'x 'y '(x z y () y x))
;Ejercicio No 09
;inversions
(define inversions
  (lambda (L)
    (if  (null? L)
         0;Caso base en: Lista vacio tiene 0 inversiones
         (count (car L)(cdr L) (inversions(cdr L))))
  )
)
(define count
  (lambda (x L acum)
         (cond
           [(null? L)acum]
           [(> x(car L))(+ 1 (count x (cdr L)acum))]
           [else (count x (cdr L) acum)]
           )
         )
  )
;Pruebas
;(inversions '(2 3 8 6 1))
;(inversions '(1 2 3 4 ))
;(inversions '(3 2 1))
;(inversions '(7 8 4 10 11 1))
;Ejercicio No 12
(define filter-acum
  (lambda (a b F acum filter)
    (if (> a b)
        acum;
        (if(filter a)
           (filter-acum (+ a 1) b F (F acum a) filter)
           (filter-acum (+ a 1) b F acum filter)
           )
        )
    )
  )
;Pruebas
;(filter-acum 1 10 + 0 odd?) ; → 25
;(filter-acum 1 10 + 0 even?) ; → 30
(define count-odd-and-even
  (lambda(arbol)
    (if (null? arbol)
        '(0 0);Caso base para cuando sea un arbol vacio
        (let* ([izq (count-odd-and-even (cadr arbol))]
               [der (count-odd-and-even (caddr arbol))]
               [nodo (car arbol)]
               [pares (+ (car izq) (car der) (if (even? nodo) 1 0))]
               [impares (+ (cadr izq) (cadr der) (if(odd? nodo ) 1 0 ))])
             (list pares impares)
          )
        )
    )
  )
;Pruebas
;(count-odd-and-even '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ()))))
;(count-odd-and-even '(8 (3 () ()) (10 () ())))
(define pascal
  (lambda (N)
    (cond
      [(= N 0) '(0)];Fila 0 en este caso no hay, ya que empezamos desde la fila 1
      [(= N 1) '(1)];Caso base donde la primera fila de Pascal es 1
      [else (next-row (pascal (- N 1)))]
      )
    )
  )
(define next-row
  (lambda (L)
    (let loop ([prev L] [result '(1)])
      (if (null? (cdr prev))
      (reverse (cons 1 result))
      (loop (cdr prev) (cons ( + (car prev)(cadr prev)) result)
      )
    )
  )
  )
)