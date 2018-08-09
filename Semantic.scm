;;; fcl semantic checker

(define-structure
	(Entry
		(constructor Entry.new (name params body . condn))
	)
	name	; symbol
	params	; [symbol]
	body	; [Node]
	condn	; [Node]
)
(define (Entry.unpack p)
	(list (entry-name p) (entry-params p) (entry-body p))
)
; semantic : [Node],[Entry]->[Entry]
(define (semantic syntax0)
	; Nodes:
	;	begin-def <name>, end-def
	;	begin-pars, end-pars
	;	begin-args, end-args
	;	begin-term, end-term
	;	begin-cond, end-cond, cond-else
	;	val <value>
	;	const <name>
	;	add, sub, mul, div

	(call/cc (lambda (return)
		; lookup : [Entry],symbol(,symbol) -> Entry
		(define (lookup table name . supername)
			;(if debug.semantic (fmt "lookup" (if (null? table) '() (entry.unpack (car table))) name supername))
			(if (not (symbol? name))
				(fmt "internal error (lookup): name must be a symbol")
				(let global-loop ((l table))   ; look up globally
					(if (null? l)
						(if (null? supername)
							#f
							(let ((entry (lookup table (car supername))))	; look up supername globally
								(cond
									((not entry) #f)
									((eq? (entry-name entry) name) entry)	; either the name was found as a global symbol
									(else
										(let param-loop ((p (entry-params entry)))	; or we step through the parameters
											(if (null? p)
												#f
												(if (eq? (car p) name)
													(entry.new '$pseudo '() '())	; and return a pseudo entry
													(param-loop (cdr p))
												)
											)
										)
									)
								)
							)
						)
						(if (eq? (entry-name (car l)) name)
							(car l)
							(global-loop (cdr l))
						)
					)
				)
			)
		)
		; insert : [Entry],symbol,Entry -> [Entry]
		(define (insert table name entry)
			(let loop ((l table))
				;(if debug.semantic (fmt "insert" name value "=" l))
				(let ((e (entry.new name (entry-params entry) (entry-body entry))))
					(if (null? l)
						(cons e '())
						(if (eq? (entry-name (car l)) name)
							(cons e (cdr l))
							(cons (car l) (loop (cdr l)))
						)
					)
				)
			)
		)

		; get-params : [Node] -> [Node],[symbol]
		(define (get-params s0)
			(let get-params-loop ((s s0) (p '()))
				(if debug.semantic (fmt "get-params" (node.unpack (car s)) p))
				(let ((nodetype (node-name (car s))) (parname (node-op (car s))))
					(case nodetype
						((begin-pars) (get-params-loop (cdr s) p)) ; skip starting node
						((end-pars) (cons (cdr s) (reverse p)))
						((const) 
							(if (not (contains p parname))
								(get-params-loop (cdr s) (cons parname p))
								(begin
									(fmt "error: duplicate parameter" parname)
									'!semantic-error
								)
							)
						)
						(else
							(fmt "internal error: illegal syntax node" nodetype "in get-params")
							'!internal-error
						)
					)
				)
			)
		)
		(define (check-args s0 p table proc)
			(define pc (length p))
			(let check-args-loop ((s s0) (c 0))
				(if debug.semantic (fmt "check-args" (node.unpack (car s)) p c))
				(let ((type (node-name (car s))) (value (node-op (car s))))
					(case type
						((begin-args) 
							(fmt "internal error: uncaught begin-args in check-args")
							(return '!internal-error)
						)
						((end-args)
							(if (= pc c)
								(cons #t (cdr s))
								(cons #f '())
							)
						)
						((const) 
							(let ((e1 (lookup table value proc)))
								(if e1
									(let* ((p1 (entry-params e1)) (r1 (check-args (cddr s) p1 table proc)))
										(if (car r1)
											(check-args-loop (cdr r1) (+ c 1))
											(cons #f value)
										)
									)
									(begin
										(fmt "semantic error: reference to unknown constant" value)
										(return '!semantic-error)
									)
								)
							)
						)
						((val)
							(check-args-loop (cdr s) (+ c 1))
						)
						((add sub mul div) (check-args-loop (cdr s) (- c 1)))
					)
				)
			)
		)
		; get-term : [Node],[Entry],symbol,[] -> [Node],[symbol]
		(define (get-term s0 table0 proc0 t0)
			(let get-term-loop ((s s0) (table table0) (proc proc0) (t t0) (tc 0))
				(if debug.semantic (fmt "get-term" (node.unpack (car s)) tc))
				(let ((nodetype (node-name (car s))) (value (node-op (car s))))
					(case nodetype
						((begin-term) (get-term-loop (cdr s) table proc t (+ tc 1)))
						((end-term)
							(if (= tc 1)
								(cons (cdr s) (reverse t))
								(get-term-loop (cdr s) table proc t (- tc 1))
							)
						)
						((const)
							(let ((entry (lookup table value proc)))
								(if entry
									(if (eq? (node-name (cadr s)) 'begin-args)
										(let ((ca (check-args (cddr s) (entry-params entry) table proc)))
											(if (list? ca)
												(if (car ca)
													(get-term-loop (cdr ca) table proc (cons (car s) t) tc)
													(begin
														(fmt "argument list does not match parameters of" (if (null? (cdr next)) value (cdr next)))
														(return '!semantic-error)
													)
												)
												ca
											)
										)
										(begin
											(fmt "internal error: const node without args")
											(return '!internal-error)
										)
									)								
									(begin
										(fmt "error: reference to unknown constant" value)
										(return '!semantic-error)
									)
								)
							)
						)
						((val add sub mul div) (get-term-loop (cdr s) table proc (cons (car s) t) tc))
						(else
							(fmt "internal error: illegal syntax node" nodetype "in get-term")
							(return '!internal-error)
						)
					)
				)
			)
		)
		; get-condn : [Node] -> [Node],[Node]
		(define (get-condn s0)
			(let get-condn-loop ((s s0) (c '()))
				(if debug.semantic (fmt "get-condn" (node.unpack (car s))))
				(let ((nodetype (node-name (car s))) (value (node-op (car s))))
					(case nodetype
						((begin-cond) (get-condn-loop (cdr s) c))
						((end-cond)
							(cons (cdr s) (reverse c)))
						((const val eq ne gt lt)
							(get-condn-loop (cdr s) (cons (car s) c))
						)
						((cond-else) (get-condn-loop (cdr s) c))
						((begin-term)
							; no guard given: return
							(cons s0 '())
						)
						(else
							(fmt "internal error: illegal syntax node" nodetype "in get-condn")
							(return '!internal-error)
						)
					)
				)
			)
		)
		(define (find-next s0 n fail)
			(let goto ((s s0))
				(cond 
					((eq? (node-name (cadr s)) n) (cdr s))
					((eq? (node-name (cadr s)) fail) #f)
					(else (goto (cdr s)))
				)
			)
		)

		(define preloaded 
			(let semantic-preload ((syntax syntax0) (table '()))
				(if (null? syntax)
					table
					(let ((x (node-name (car syntax))) (y  (node-op (car syntax))))
						(case x
							((begin-def)
								(if (not (lookup table y))
									(let* ((r (get-params (cdr syntax))) (s (car r)) (p (cdr r)))
										(semantic-preload s (insert table y (entry.new y p '())))
									)
									(begin
										(fmt "error: redefinition of function" y)
										(return '!semantic-error)
									)
								)
							)
							(else (semantic-preload (cdr syntax) table))
						)
					)
				)
			)
		)
		(let semantic-loop ((syntax syntax0) (table preloaded))
			(if debug.semantic
				(fmt "semantic" (if (null? syntax) '() (node.unpack (car syntax))) table)
			)
			(if (null? syntax)
				(begin
					(fmt "semantic analysis succeeded, created" (length table) "table entries")
					table
				)
				(let ((x (node-name (car syntax))) (y  (node-op (car syntax))))
					(case x
						((begin-def)
							(let* 	((pars (entry-params (lookup table y)))
									(r2 
										(let ((s1 (find-next (cdr syntax) 'begin-cond 'begin-term)))
											(if s1
												(get-condn s1)
												(cons (find-next (cdr syntax) 'begin-term '()) '())
											)
										)
									)
									(s2 (car r2)) (condn (cdr r2)) (t2 (insert table y (entry.new y pars '() condn)))
									(r3 (get-term s2 t2 y '())) (s3 (car r3)) (body (cdr r3)) (t3 (insert table y (entry.new y pars body condn)))
									)
								(if (eq? (node-name (car s3)) 'end-def)
									(semantic-loop (cdr s3) t3)
									(begin
										(fmt "internal error: unterminated function" y)
										(return '!internal-error)
									)
								)
							)
						)
						(else
							(fmt "unknown symbol" x)
							(return '!internal-error)
						)
					)
				)
			)
		)
	))
)