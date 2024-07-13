
(define-module (microk-fo)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (common)
  #:export (<disj> disj <conj> conj <relate> relate <==> ==
                   =/=
                   <symbolo> symbolo
                   <stringo> stringo
                   <numbero> numbero
                   <not-symbolo> not-symbolo
                   <not-stringo> not-stringo
                   <not-numbero> not-numbero
                   <mplus> mplus
                   <bind> bind
                   <pause> mk-pause
                   step
                   mature
                   mature?))

;; first-order microKanren
(define-record-type <disj>
  (disj g1 g2)
  disj?
  (g1 disj-g1)
  (g2 disj-g2))

(define-record-type <conj>
  (conj g1 g2)
  conj?
  (g1 conj-g1)
  (g2 conj-g2))

(define-record-type <relate>
  (relate thunk description)
  relate?
  (thunk relate-thunk)
  (description relate-description))

(define-record-type <==>
  (== t1 t2)
  ==?
  (t1 ==-t1)
  (t2 ==-t2))

(define-record-type <=/=>
  (=/= t1 t2)
  =/=?
  (t1 =/=-t1)
  (t2 =/=-t2))

(define-record-type <symbolo>
  (symbolo t)
  symbolo?
  (t symbolo-t))

(define-record-type <stringo>
  (stringo t)
  stringo?
  (t stringo-t))

(define-record-type <numbero>
  (numbero t)
  numbero?
  (t numbero-t))

(define-record-type <not-symbolo>
  (not-symbolo t)
  not-symbolo?
  (t not-symbolo-t))

(define-record-type <not-stringo>
  (not-stringo t)
  not-stringo?
  (t not-stringo-t))

(define-record-type <not-numbero>
  (not-numbero t)
  not-numbero?
  (t not-numbero-t))

(define-record-type <bind>
  (bind s g)
  bind?
  (s bind-s)
  (g bind-g))

(define-record-type <mplus>
  (mplus s1 s2)
  mplus?
  (s1 mplus-s1)
  (s2 mplus-s2))

(define-record-type <pause>
  (mk-pause state goal)
  pause?
  (state pause-state)
  (goal pause-goal))

(define (mature? s) (or (not s) (pair? s)))
(define (mature s)
  (if (mature? s) s (mature (step s))))

(define (start st g)
  (match g
    (($ <disj> g1 g2)
     (step (mplus (mk-pause st g1)
                  (mk-pause st g2))))
    (($ <conj> g1 g2)
     (step (bind (mk-pause st g1) g2)))
    (($ <relate> thunk _)
     (mk-pause st (thunk)))
    (($ <==> t1 t2) (unify t1 t2 st))
    (($ <=/=> t1 t2) (state->stream (disunify t1 t2 st)))
    (($ <symbolo> t) (state->stream (typify t symbol? st)))
    (($ <stringo> t) (state->stream (typify t string? st)))
    (($ <numbero> t) (state->stream (typify t number? st)))
    (($ <not-symbolo> t) (state->stream (distypify t symbol? st)))
    (($ <not-stringo> t) (state->stream (distypify t string? st)))
    (($ <not-numbero> t) (state->stream (distypify t number? st)))
    ))

(define (step s)
  (match s
    (($ <mplus> s1 s2)
     (let ((s1 (if (mature? s1) s1 (step s1))))
       (cond ((not s1) s2)
             ((pair? s1)
              (cons (car s1)
                    (mplus s2 (cdr s1))))
             (else (mplus s2 s1)))))
    (($ <bind> s g)
     (let ((s (if (mature? s) s (step s))))
       (cond ((not s) #f)
             ((pair? s)
              (step (mplus (mk-pause (car s) g)
                           (bind (cdr s) g))))
             (else (bind s g)))))
    (($ <pause> st g) (start st g))
    (_            s)))
