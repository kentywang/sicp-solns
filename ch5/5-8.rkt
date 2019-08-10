;; Since the order of labels (with their instructions) is front-to-back
;; ordered, goto will jump to the first here, meaning the contents of
;; register a will be 3. 

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels 
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (lookup-label labels next-inst)
                   (error "Duplicate label: EXTRACT-LABELS"
                          next-inst)
                   (receive 
                    insts
                    (cons 
                     (make-label-entry 
                      next-inst
                      insts)
                     labels)))
               (receive 
                   (cons (make-instruction 
                          next-inst)
                         insts)
                   labels)))))))