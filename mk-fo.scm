
(define-module (mk-fo)
  #:use-module (common)
  #:use-module (microk-fo)
  #:re-export (== =/= symbolo numbero stringo not-symbolo not-numbero not-stringo mk-pause)
  #:export (define-relation fresh conde query run run* stream-take conj* disj*))
(include "mk-syntax.scm")
