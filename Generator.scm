;;; fcl code generator hub

(load "GeneratorC.scm")
(define fcl-to-c 'c)

(define (generate input lang)
	(case lang
		((c) (generate-c input))
		(else 
			(fmt "unknown target language" lang)
			'!generator-error
		)
	)
)