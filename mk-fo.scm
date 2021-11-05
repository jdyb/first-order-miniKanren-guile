
(define-module (mk-fo)
  #:use-module (common)
  #:use-module (microk-fo)
  #:re-export (== mk-pause)
  #:export (fresh define-relation conde query run run* stream-take conj*
               disj*))
(include "mk-syntax.scm")
