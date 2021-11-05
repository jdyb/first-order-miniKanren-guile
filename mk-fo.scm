
(define-module (mk-fo)
  #:use-module (common)
  #:use-module (microk-fo)
  #:re-export (==)
  #:export (define-relation fresh conde query run run* stream-take conj*
               disj*))
(== 1 1)
(include "mk-syntax.scm")
