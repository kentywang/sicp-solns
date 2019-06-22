;; 3.3, unmodified except reverses password and action parameters.
(define (make-account balance pw)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-pw amount) "Incorrect password.")
  (define (dispatch m n)
    (if (eq? m pw)
        (cond ((eq? n 'withdraw) withdraw)
              ((eq? n 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           n)))
        incorrect-pw))
  dispatch)

;;; Main

(define (make-joint acct orig-pw alt-pw)
  (lambda (pw op)
    (acct (if (eq? pw alt-pw)
              orig-pw
              '())
          op)))

;;; Tests

(define peter-acc (make-account 1000 'open-sesame))

(define paul-acc
  (make-joint peter-acc
              'open-sesame
              'rosebud))

