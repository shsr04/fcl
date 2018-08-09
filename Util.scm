;;; fcl utility functions

; read-file : string->string
(define (read-file filename)
	(let ((inp (open-input-file filename)))
		(let loop ((l (read-line inp)))
			(if (eof-object? l)
				(begin
					(close-input-port inp)
					""
				)
				(string-append l (loop (read-line inp)))
			)
		) 
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