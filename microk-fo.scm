
(define-module (microk-fo)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (common)
  #:export (<disj> disj <conj> conj <relate> relate <==> == <mplus> mplus
                   <bind> bind <pause> pause step mature mature?))


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
     (step (mplus (pause st g1)
                  (pause st g2))))
    (($ <conj> g1 g2)
     (step (bind (pause st g1) g2)))
    (($ <relate> thunk _)
     (pause st (thunk)))
    (($ <==> t1 t2) (unify t1 t2 st))))

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
              (step (mplus (pause (car s) g)
                           (bind (cdr s) g))))
             (else (bind s g)))))
    (($ <pause> st g) (start st g))
    (_            s)))
