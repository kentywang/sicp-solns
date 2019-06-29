;; Works because exchange(A, B) and exchange(B, A) will both have the same
;; serialized procedure layering, meaning in the event they happen
;; concurrently, both try to acquire the lower id account's mutex and one
;; will have to wait until the other completely finishes, instead of both
;; acquiring their respective outer mutexes and reach a deadlock on trying
;; to acquire the same inner mutex.

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((if (< (account1 'id) (account2 'id))
         (serializer1 (serializer2 exchange))
         (serializer2 (serializer1 exchange)))
     account1
     account2)))

(define curr-high-id 0)

(define (make-account-and-serializer balance)
  (let ((prio (+ 1 curr-high-id)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (let ((balance-serializer
           (make-serializer)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'balance) balance)
              ((eq? m 'serializer)
               balance-serializer)
              ((eq? m 'id) prio)
              (else (error "Unknown request:
                          MAKE-ACCOUNT"
                           m))))
      dispatch)))
