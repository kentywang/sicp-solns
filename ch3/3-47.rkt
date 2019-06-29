;;; Deps

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;;; Main

(define (make-semaphore-2a n)
  (define make-mutex-list
    (lambda (x)
      (if (= x 0)
          '()
          (cons (make-mutex) (- x 1)))))
  (let ((mutexes (make-mutex-list n)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             ))) ; How do we get check mutex without getting stuck in wait?
    the-semaphore))

;; Edit: I'm thinking of the problem in the wrong way. Instead of creating n
;; mutexes, I really just need one mutex and use it to atomically set a variable
;; for the number of free spots left. Think of a mutex as a way to stop time and
;; perform an action.

(define (make-sempaphore-2b n)
  (define (make-list-length n)
    (if (= n 0)
        '()
        (cons false (make-list-length (- n 1)))))
  (let ((cells (make-list-length n)))
    (define (the-mutexes m)
      (define (iter c)
        (if (null? c)
            (iter cells) ; Restart the loop once we reach the end.
            (if ((cond ((eq? m 'acquire)
                        test-and-set!)
                       ((eq? m 'release)
                        test-and-clear?)) ; Assuming I have this proc. We need
                 cell)                    ; to ensure clear is atomic to prevent
                (iter (cdr cells)))))     ; losing usable mutexes.
      (iter cells))
    the-mutexes))
