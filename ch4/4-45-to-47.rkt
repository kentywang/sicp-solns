#lang sicp

;;; 4.45

;;; Deps

(define (require p)
  (if (not p) (begin (display (list 'Failed! p))
                     (newline)
                     (amb))))

;;; Word Lists

(define nouns 
  '(noun student professor cat class))

(define verbs 
  '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions 
  '(prep for to in by with))

;;; Code

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) 
                 (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define (parse-prepositional-phrase)
  (display "Prep ")
  (display *unparsed*)
  (newline)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (display (list "Verb amb" verb-phrase))
    (newline)
    (amb 
     verb-phrase
     (maybe-extend 
      (list 'verb-phrase
            verb-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (display (list "Noun amb" noun-phrase))
    (newline)
    (amb 
     noun-phrase
     (maybe-extend 
      (list 'noun-phrase
            noun-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

;;; Tests

(parse '(the professor lectures to the student in the class with the cat))

;; Observations: the code is actually quite deceiving. The ambs for the parse-
;; noun-phrase and parse-verb-phrase actually get called AFTER its preposition
;; amb is processed, meaning that we don't quite do a depth-first search. This
;; behavior is quite necessary because DFS would actually cause the search to
;; continuously try parsing nested prepositional phrases, so it would never
;; quite backtrack to try other possiblities earlier in the chain.

;; Local testing fails to find other paths because the amb isn't reversing
;; the assignments like it's supposed to when trying a different route.

;; Actually, I still don't quite get how the amb works. Even with the
;; wrapping amb from the parent component, it doesn't look like it should
;; backtrack.

;; Hmm, I think we don't get an infinite recursion down the first verb phrase
;; parsing because on the first backtrack we don't reverse any assignments, so
;; the *unparsed* is still empty. Therefore, when we try and parse the overly
;; nested verb phrase's preposition, we are halted before calling another amb,
;; so then we backtrack out of the verb phrase and into its child prop's noun's
;; amb fork.

;; 1. Lecturing to the student, lecturing in the class, lecturing with the cat.
(sentence
 (simple-noun-phrase
  (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb-phrase
    (verb lectures)
    (prep-phrase
     (prep to)
     (simple-noun-phrase
      (article the) (noun student))))
   (prep-phrase
    (prep in)
    (simple-noun-phrase
     (article the) (noun class))))
  (prep-phrase
   (prep with)
   (simple-noun-phrase
    (article the) (noun cat)))))

;; 2. Lecturing to the student who is in class, lecturing with the cat.
(sentence
 (simple-noun-phrase
  (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb lectures)
   (prep-phrase
    (prep to)
    (noun-phrase
     (simple-noun-phrase
      (article the) (noun student))
     (prep-phrase
      (prep in)
      (simple-noun-phrase
       (article the) (noun class))))))
  (prep-phrase
   (prep with)
   (simple-noun-phrase
    (article the) (noun cat)))))

;; 3. Lecturing to the student who is in class, the class with the cat.
(sentence
 (simple-noun-phrase
  (article the) (noun professor))
  (verb-phrase
   (verb lectures)
   (prep-phrase
    (prep to)
    (noun-phrase
     (simple-noun-phrase
      (article the) (noun student))
     (prep-phrase
      (prep in)
      (noun-phrase
       (simple-noun-phrase
        (article the) (noun class))
       (prep-phrase
        (prep with)
        (simple-noun-phrase
         (article the) (noun cat)))))))))

;; 4. Lecturing to the student, lecturing in the class, the class with the cat.
(sentence
 (simple-noun-phrase
  (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb lectures)
   (prep-phrase
    (prep to)
     (simple-noun-phrase
      (article the) (noun student))))
  (prep-phrase
   (prep in)
   (noun-phrase
    (simple-noun-phrase
     (article the) (noun class))
    (prep-phrase
     (prep with)
     (simple-noun-phrase
      (article the) (noun cat)))))))

;; 5. Lecturing to the student who is in class, the student with the cat.
(sentence
 (simple-noun-phrase
  (article the) (noun professor))
  (verb-phrase
   (verb lectures)
   (prep-phrase
    (prep to)
    (noun-phrase
     (noun-phrase
      (simple-noun-phrase
       (article the) (noun student))
      (prep-phrase
       (prep in)
       (simple-noun-phrase
        (article the) (noun class)))
      (prep-phrase
       (prep with)
       (simple-noun-phrase
        (article the) (noun cat))))))))

;;; 4.46

;; If evaluation of amb happens RTL, then we enter an infinite recursion
;; down maybe-extend.

;; More generally, if we have our entire evaluator RTL, then we start with
;; parsing verbs, and that would fail in a SVO language.

;; 4.47

;; His change would make the evaluator try the verb WHILE evaluating the amb.
;; so the assignment is backtracked if the first option fails. But then the
;; second option includes a recursive call to the parsing procedure which then
;; makes the same mistake again, on another layer to the recursive call stack.

;; The original approach deals with this by tryingt the verb BEFORE the amb, so
;; if it fails, the backtracking does not reverse the assignment so we can use
;; it and look forward in the sentence.

;; Also interchanging the order of expressions in the amb is more obviously
;; flawed; we enter a simple infinite recursive loop.