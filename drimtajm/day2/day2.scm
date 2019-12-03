(define readmem
  (lambda (currentpos targetpos opcodes)
    (if (= currentpos targetpos)
	(car opcodes)
	(readmem (+ currentpos 1) targetpos (cdr opcodes)))))

(define compute
  (lambda (opcode term1 term2)
    (cond ((eq? opcode 1) (+ term1 term2))
	  ((eq? opcode 2) (* term1 term2)))))

(define replacecode
  (lambda (currentpos opcodes targetpos value)
    (cond ((< targetpos currentpos) opcodes)
	  ((= targetpos currentpos) (cons value (cdr opcodes)))
	  ((> targetpos currentpos)
	   (cons (car opcodes)
		 (replacecode (+ currentpos 1) (cdr opcodes)
			      targetpos value))))))

(define intcode
  (lambda (opcodes position allcodes)
    (if (eq? (car opcodes) 99)
	allcodes
	(let* ((term1 (readmem 0 (cadr opcodes) allcodes))
	       (term2 (readmem 0 (caddr opcodes) allcodes))
	       (result (compute (car opcodes) term1 term2))
	       (targetpos (cadddr opcodes))
	       (nextposition (+ position 4))
	       (nextopcodes (replacecode nextposition (cddddr opcodes)
					 targetpos result))
	       (nextallcodes (replacecode 0 allcodes targetpos result)))
	  (intcode nextopcodes nextposition nextallcodes)))))

(define runintcode
  (lambda (opcodes)
    (intcode opcodes 0 opcodes)))

(define tryallcodes
  (lambda (noun verb inputcodes targetresult)
    (let* ((opcodes (replacecode 0 (replacecode 0 inputcodes 1 noun) 2 verb))
	   (actualresult (car (runintcode opcodes))))
      (cond ((= actualresult targetresult) (+ (* noun 100) verb))
	    ((= verb 99) (tryallcodes (+ noun 1) 0 inputcodes targetresult))
	    (else (tryallcodes noun (+ verb 1) inputcodes targetresult))))))

(let ((inputcodes '(1 0 0 3
		      1 1 2 3
		      1 3 4 3
		      1 5 0 3
		      2 10 1 19
		      1 19 9 23
		      1 23 13 27
		      1 10 27 31
		      2 31 13 35
		      1 10 35 39
		      2 9 39 43
		      2 43 9 47
		      1 6 47 51
		      1 10 51 55
		      2 55 13 59
		      1 59 10 63
		      2 63 13 67
		      2 67 9 71
		      1 6 71 75
		      2 75 9 79
		      1 79 5 83
		      2 83 13 87
		      1 9 87 91
		      1 13 91 95
		      1 2 95 99
		      1 99 6 0
		      99
		      2 14 0 0)))
  (print (runintcode (replacecode 0 (replacecode 0 inputcodes 1 12) 2 2)))
  (print (tryallcodes 0 0 inputcodes 19690720)))

