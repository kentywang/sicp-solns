(define (stream-map proc . argstreams)
  (if (null? (car argstreams)) ; or stream-null?
      the-empty-stream
      (cons-stream
       (apply proc (map car argstreams)) ; or stream-car?
       (apply stream-map
              (cons proc 
                    (map stream-cdr ; Can't be cdr, as that returns the lambda, not the cons-stream.
                         argstreams))))))