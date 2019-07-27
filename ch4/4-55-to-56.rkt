4.55

1. (supervisor ?name (Bitdiddle Ben))
2. (job ?name (accounting . ?position))
3. (address ?name (Slumerville . ?addr))

4.56

1. (and (supervisor ?name (Bitdiddle Ben))
        (address ?name ?addr))
2. (and (salary (Bitdiddle Ben) ?bsal)
        (salary ?person ?psal)
        (lisp-value > ?bsal ?psal))
3. (and (supervisor ?person ?super)
        (job ?super ?job)
        (not (job ?super (computer . ?pos))))