(define f
  (let ((saved '()))
    (lambda (x)
      (if (null? saved)
          (begin (set! saved x)
                 saved)
          saved))))
