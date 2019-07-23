#lang racket

;; Note: taken from to https://gitlab.com/barry.allison/wizard-book-solutions
;; in order to run my own code

;; Can't rely on swindle's version of amb any more,
;; so implement ambeval given by the book's web site
;; https://mitpress.mit.edu/sicp/code/index.html
;; Have to modify it somewhat to use the base metacircular evaluator in base-eval.rkt
;; and to use racket's mutable cons cells/lists. 
(require "amb-eval-dep1.rkt")

(interpret '(define (require p) (if (not p) (amb))))

(interpret '(define nouns '(noun student professor cat class)))
(interpret '(define verbs '(verb studies lectures eats sleeps)))
(interpret '(define articles '(article the a)))
(interpret '(define prepositions '(prep for to in by with)))

;; 4.48
(interpret '(define conjunctions '(conj and or)))

;; 4.48
(interpret '(define (parse-sentence)
              (define (maybe-extend sent)
                (amb sent
                     (maybe-extend
                      (list 'compound-sentence
                            sent
                            (parse-word conjunctions)
                            (parse-sentence)))))
              (maybe-extend
               (list 'sentence
                     (parse-noun-phrase)
                     (parse-verb-phrase)))))

(interpret '(define (parse-noun-phrase)
              (define (maybe-extend noun-phrase)
                (amb noun-phrase
                     (maybe-extend (list 'noun-phrase
                                         noun-phrase
                                         (parse-prepositional-phrase)))))
              (maybe-extend (parse-simple-noun-phrase))))

(interpret '(define (parse-simple-noun-phrase)
              (list 'simple-noun-phrase
                    (parse-word articles)
                    (parse-word nouns))))

(interpret '(define (parse-verb-phrase)
              (define (maybe-extend verb-phrase)
                (amb verb-phrase
                     (maybe-extend (list 'verb-phrase
                                         verb-phrase
                                         (parse-prepositional-phrase)))))
              (maybe-extend (parse-word verbs))))

(interpret '(define (parse-prepositional-phrase)
              (list 'prep-phrase
                    (parse-word prepositions)
                    (parse-noun-phrase))))

(interpret '(define (parse-word word-list)
              (require (not (null? *unparsed*)))
              (require (memq (car *unparsed*) (cdr word-list)))
              (let ((found-word (car *unparsed*)))
                (set! *unparsed* (cdr *unparsed*))
                (list (car word-list) found-word))))

(interpret '(define *unparsed* '()))
(interpret '(define (parse input)
              (set! *unparsed* input)
              (let ((sent (parse-sentence)))
                (require (null? *unparsed*))
                sent)))

(driver-loop)

;;; Tests

;(parse '(the professor lectures to the student with the cat))
;(parse '(the professor lectures and the class eats))
;(parse '(the professor lectures to the student and the student sleeps with the cat or the class eats))

;; Edit: adjectives are adverbs aren't apparently hard either. I just need to have
;; an amb without and with the adj/adv. I need to recompute article and noun or
;; the verb in the second path of the amb though, since the order is vital to
;; the parsing and mutation.

;; http://community.schemewiki.org/?sicp-ex-4.48