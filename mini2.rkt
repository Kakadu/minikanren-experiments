#lang racket

(provide run run*
         == =/=
         fresh
         conde
         symbolo numbero
         absento
         (all-defined-out))

;; extra stuff for racket
;; due mostly to samth
(define (list-sort f l) (sort l f))

(define (remp f l) (filter-not f l))

(define (call-with-string-output-port f)
  (define p (open-output-string))
  (f p)
  (get-output-string p))

(define (exists f l) (ormap f l))

(define for-all andmap)

(define (find f l)
  (cond [(memf f l) => car] [else #f]))

(define memp memf)

(define (var*? v) (var? (car v)))


; Substitution representation

(define empty-subst-map (hasheq))

(define subst-map-length hash-count)

; Returns #f if not found, or a pair of u and the result of the lookup.
; This distinguishes between #f indicating absence and being the result.
(define subst-map-lookup
  (lambda (u S)
    (hash-ref S u unbound)))

(define (subst-map-add S var val)
  (hash-set S var val))

(define subst-map-eq? eq?)


; Constraint store representation

(define empty-C (hasheq))

(define set-c
  (lambda (v c st)
    (state (state-S st) (hash-set (state-C st) v c))))

(define lookup-c
  (lambda (v st)
    (hash-ref (state-C st) v empty-c)))

(define remove-c
  (lambda (v st)
    (state (state-S st) (hash-remove (state-C st) v))))


(include "faster-miniKanren/mk.scm")

(define appendo
  (lambda (l s out)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))])))

;(printf "testing appendo\n")
;(run* (q) (appendo q '(a b c) '(a b c) ))

;(define reverso
;  (lambda (xs out)
;    (conde
;      [(== xs `()) (== out `() ) ]
;      [(fresh (h t aa)
;          (== `(,h . ,t) xs)
;          (appendo aa `(,h) out)
;          (reverso t aa)
;          )
;       ]
;    )
;  )
;)
(define conso (lambda (h tl out) (== (cons h tl) out)))
(define eq (lambda (a b) (== a b)))
(define list_eq_naive
  (lambda (xs ys)
    ;(printf "list_eq_naive: ~a ~a~n" xs ys)
    (== ys xs)))
  
(define list_eq
  (lambda (xs ys)
    ;(printf "list_eq: ~a ~a~n" xs ys)
    (conde [(== '() xs) (== '() ys)]
           [(fresh (h1 t1 t2)
                   (== `(,h1 . ,t1) xs)
                   (== `(,h1 . ,t2) ys)
                   (list_eq t1 t2)
                   )])))

(run 1 (q) (list_eq_naive '(1) '(1) ))
(run 1 (q) (list_eq_naive '(1)   q ))
(run 1 (q) (list_eq_naive '(1) `(,q) ))

(run 1 (q) (list_eq '(1) '(1) ))
(run 1 (q) (list_eq '(1)   q ))
(run 1 (q) (list_eq '(1) `(,q) ))

(define reverso_helper
  (lambda (xs acc out)
    ;(printf "reverso_helper: ~a ~a ~a~n" xs acc out)
    (conde
      [(== '() xs) (list_eq acc out)]
      ;[(== '() xs) (== acc out)]
      [(fresh (h t)
        (== `(,h . ,t) xs)
        (reverso_helper t `(,h . ,acc) out)
        )]
    ))
  )

(define reverso (lambda (xs ys) (reverso_helper xs `() ys)) )

(printf "calling reverso with run 1\n")
; we call run 1 because there is only 1 solution and if we call with >1 it hangs
(run 1 (q) (reverso '(a b c)  q) ) ; works
(run 1 (q) (reverso q '(a b c) ) ) ; works

(printf "calling reverso with run*\n")
(run* (q) (reverso '(a b c)  q) )   ; works
;(run* (q) (reverso q '(a b c) ) )   ; hangs

(define reverso1
  (lambda (a b)
    (conde
      [(== a '()) (== b '())]
      [(fresh (h t)
             (== a `(,h . ,t))
             (fresh (aa)
                      (appendo aa `(, h) b)
                      (reverso1 t aa)
                      
             )
       )]
    )
  )
)

(printf "calling reverso1 with run1\n")
; this finishes too
(run 1 (q) (reverso1 q '(a b c) ) )
(run 1 (q) (reverso1 '(a b c) q) )

(printf "calling reverso1 with run*\n")
; this finishes too
(run* (q) (reverso1 q '(a b c) ) )    ; works
; (run* (q) (reverso1 '(a b c) q) )     ; hangs

; so, in minikanren both implementations of reverso work. somehow....

;(run* (q) (conso q '(2 3) '(2 3)) )
;(run* (q) (eq q '(2 3)) )



