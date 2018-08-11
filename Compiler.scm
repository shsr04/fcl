(define debug.scan #f)
(define debug.parse #f)
(define debug.semantic #f)
(define debug.generate #f)
(load "Util.scm")
(load "Scanner.scm")
(load "Parser.scm")
(load "Semantic.scm")
(load "Generator.scm")

;;; engage triangle mode

; compile : string,string,symbol->(file)
(define (compile filename outname lang)
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
											(if (or debug.semantic debug.generate) (fmt "=> table =" (map entry.unpack table)))
											(let ((output (generate table lang)))
												(if debug.generate (fmt "=> code =" output))
												(write-file outname output)
												(fmt "the output has been written to" outname)
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
)