;;; fcl utility functions

; read-file : string->string
(define (read-file filename)
	(let* ((inp (open-input-file filename)) (r (read-string (char-set) inp)))
		(close-input-port inp)
		r
	)
)

; write-file : string,string->[]
(define (write-file filename content)
	(let ((outp (open-output-file filename)))
		(write-string content outp)
		(close-output-port outp)
		'()
	)
)

(define (fmt . x)
	(display x)
	(display "\n")
)

(define call/cc call-with-current-continuation)

(define (contains l x) ;;; deprecated, use memq/memv/member instead
	(if (or (not (list? l)) (null? l))
		#f
		(if (equal? (car l) x)
			#t
			(contains (cdr l) x)
		)
	)
)