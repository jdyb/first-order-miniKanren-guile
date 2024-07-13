
(define-module (mk-ho)
  #:use-module (common)
  #:use-module (microk-ho)
  #:re-export (== =/= symbolo numbero stringo not-symbolo not-numbero not-stringo mk-pause)
  #:export (define-relation fresh conde query run run* stream-take conj* disj*))
(include "mk-syntax.scm")
