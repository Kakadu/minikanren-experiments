
#lang racket

; microkanren in single file
(define (assp p l)
  (if (equal? l '())
      #f
      (if (p (caar l)) (car l) (assp p (cdr l)))))

(define errorf error)

;; Jason Hemann and Dan Friedman
;; microKanren, final implementation from paper

(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s)
  (printf "ext-s ~a -> ~a IN ~a~n" x v s)
  `((,x . ,v) . ,s)
  )

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))
;;;; How to make a simple miniKanren (substitution only)

(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh
      (lambda (x0)
        (fresh (x ...) g0 g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (map reify-1st (take n (call/goal (fresh (x ...) g0 g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (map reify-1st (take-all (call/goal (fresh (x ...) g0 g ...)))))))

(define empty-state '(() . 0))

(define (call/goal g) (g empty-state))

(define (pull $)
  (if (procedure? $) (pull ($)) $))

(define (take-all $)
  (let (($ (pull $)))
    (if (null? $) '() (cons (car $) (take-all (cdr $))))))

(define (take n $)
  (if (zero? n) '()
    (let (($ (pull $)))
      (if (null? $) '() (cons (car $) (take (- n 1) (cdr $)))))))

(define (reify-1st s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v '()))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (walk* (car v) s)
                   (walk* (cdr v) s)))
      (else  v))))

(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
      ((var? v)
       (let  ((n (reify-name (length s))))
         (cons `(,v . ,n) s)))
      ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
      (else s))))

(define (reify-name n)
  (string->symbol
    (string-append "_" "." (number->string n))))

(define (fresh/nf n f)
  (letrec
    ((app-f/v*
       (lambda (n v*)
         (cond
           ((zero? n) (apply f (reverse v*)))
           (else (call/fresh
                   (lambda (x)
                     (app-f/v* (- n 1) (cons x v*)))))))))
     (app-f/v* n '())))

;;; Test programs

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

(define (appendo l s out)
  (printf "appendo ~a ~a ~a~n" l s out)
  (conde
    ((== '() l) (== s out))
    ((fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res)))))

; (printf "testing appendo~n")
; (run* (q) (fresh (x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5))))
(define (test_app1 ans)
 (call/fresh (lambda (q)
   (call/fresh (lambda (w)
     (call/fresh (lambda (e)
       (conj (== ans `(,q . ,w . ,e)) (appendo q `(,w) e )) ) ) )))))

;(run 5 (ans) (test_app1 ans))

;(run 5 (w q) (appendo w `(1) q ))
;(run 5 (q w) (appendo w `(1) q ))
;(exit 0)

(define (reverso0 a b)
  (printf "reverso0 ~a ~a~n" a b)
  (disj (conj (== a '()) (== b '()))
    (call/fresh
      (lambda (h)
        (call/fresh
          (lambda (t)
             (conj (== a `(,h . ,t))
                   (call/fresh
                     (lambda (aa)
                       (conj (appendo aa `(,h) b)
                             (reverso0 t aa)))))))))))

;(printf "testing reverso0~n")
;(run* (q) (reverso0 q '(1) ))
;(run 5 (q) (reverso0 `(1) q ))

;(exit 0)

(define (reverso a b)
  (conde
    ((== '() a) (== '() b))
    ((fresh (h t aa)
       (conj (== `(,h . ,t) a) (conj (appendo aa '(h) b) (reverso t aa) ) )

       ))))

(define (conso x xs ys) (== (cons x xs) ys) )

(define (reverso1 a b)
  (conde
    [(== a '()) (== b '())]
    ((fresh (h t)
       (conj (conso h t a)
             (fresh (aa)
                (conj (appendo aa '(,h) b)     ; swapping this line and next causes loop
                      (reverso1 t aa))))))))

(define (reverso1_expanded a b)
  (printf "reverso1_expanded ~a ~a~n" a b)
  (disj
   (Zzz (conj+ (== a '()) (== b '())))
   (disj+
    (conj+
     (fresh (h)
            (fresh (t)
                   (conj
                    (conso h t a)
                    (fresh
                     (aa)
                     (conj (appendo aa '(h) b) (reverso1_expanded t aa))))))))) )


(define (reverso_db a b)
  (call/fresh
   (lambda (h)
     (call/fresh
      (lambda (t)
        (call/fresh
         (lambda (aa)
           (disj (conj (== a '()) (== b '()))
                 (conj (== a `(,h . ,t))
                       (conj (appendo aa `(,h) b)
                             (reverso_db t aa)))))))))))

;(printf "testing reverso1~n")
;(run* (q) (reverso1_expanded q '(1) ))
;(run* (q) (reverso1 q '(1) ))

(printf "testing reverso_db~n")
(run 1 (q) (reverso_db q '(1 2 3) ))
(run 1 (q) (reverso_db '(1 2 3) q))     ; hangs

;(run* (q) (reverso1  q '(1) ))
;((call/fresh (lambda (q) (reverso0  '(1)  q) )) empty-state)

;(run* (q) (== q 3))             ; succeedes
;(run* (q) (== `(q) `(3) ))      ; fails
