(define debug.scan #f)
(define debug.parse #f)
(define debug.semantic #f)
(load "Util.scm")
(load "Scanner.scm")
(load "Parser.scm")
(load "Semantic.scm")

;;; engage triangle mode

; compile : string->[file]
(define (compile filename)
	(display "\n")
	(let ((input (read-file filename)))
		(let ((tokens (scan input)))
			(if (eq? tokens '!scan-error)
				#f
				(begin
					(if (or debug.scan debug.parse) (begin (fmt "=> tokens =" (map token.unpack tokens))))
					(let ((syntax (parse tokens)))
						(if (eq? syntax '!syntax-error)
							#f
							(begin
								(if (or debug.parse debug.semantic) (fmt "=> syntax =" (map node.unpack syntax)))
								(let ((table (semantic syntax)))
									(if (eq? table '!semantic-error)
										#f
										(begin
											(if debug.semantic (fmt (map entry.unpack table)))
											#t
										)
									)
								)
							)
						)
					)
				)
			)
		)
	)
)

(display (compile "file1.fcl"))

