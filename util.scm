
(define-module (util)
  #:export (assf))

(define (assf f al)
  (if (procedure? f)
      (if (and (pair? al) (pair? (car al)))
          (if (f (caar al))
              (car al)
              (assf f (cdr al)))
          (if (null? al)
              #f
              (error 'assf-bad-association-list al)))
      (error 'assf-expected-procedure f)))

