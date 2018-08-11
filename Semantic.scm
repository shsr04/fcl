;;; fcl semantic checker

(define-structure
	(Entry
		(constructor Entry.new (name params body condn))
	)
	name	; symbol
	params	; [symbol]
	body	; [Node]
	condn	; [Node]
)
(define (Entry.unpack p)
	(list 
		"name: " (entry-name p) 
		", params: " (entry-params p) 
		", body: " (map node.unpack (entry-body p)) 
		", condn: " (map node.unpack (entry-condn p)))
)
; lookup : [Entry],symbol(,symbol) -> Entry
(define (lookup table name . supername)
;(if debug.semantic (fmt "lookup" (if (null? table) '() (map entry.unpack table)) name supername))
	(if (not (symbol? name))
		(begin
			(fmt "internal error (lookup): name must be a symbol")
			'!internal-error
		)
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
											(entry.new '$pseudo '() '() '())	; and return a pseudo entry
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
		(let ((e (entry.new name (entry-params entry) (entry-body entry) (entry-condn entry))))
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
; find-next : [Node],symbol,symbol -> [Node]|boolean
(define (find-next s0 n fail)
(let goto ((s s0))
	(cond
		((or (null? s) (null? (cdr s))) #f)
		((eq? (node-name (cadr s)) n) (cdr s))
		((eq? (node-name (cadr s)) fail) #f)
		(else (goto (cdr s)))
	)
)
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
		; get-args : [Node],[symbol],[Entry],symbol,symbol -> [Node],[Node]
		(define (get-args s0 p table proc callee)
			(define pc (length p))
			(let get-args-loop ((s s0) (a '()) (ac 0))
				(if debug.semantic (fmt "get-args" (node.unpack (car s)) p ac))
				(let ((type (node-name (car s))) (value (node-op (car s))))
					(case type
						((begin-args)
							(get-args-loop (cdr s) (cons (car s) a) ac)
						)
						((end-args)
							(if (= ac pc)
								(cons (cdr s) (cons (car s) a))
								(begin
									(fmt "argument list does not match parameters of " callee)
									(return '!semantic-error)
								)
							)
						)
						((begin-term) 
							(let* ((r1 (get-term s table proc)) (s1 (car r1)) (t (cdr r1)))
								(get-args-loop s1 (cons (node.new 'end-term '()) (append t (cons (car s) a))) (+ ac 1))
							)
						)
						(else 
							(fmt "internal error: unknown node type" type "in get-args")
							(return '!internal-error)
						)
					)
				)
			)
		)
		; get-term : [Node],[Entry],symbol -> [Node],[Node]
		(define (get-term s0 table proc)
			(let get-term-loop ((s s0) (t '()) (td 0))
				(if debug.semantic (fmt "get-term" (node.unpack (car s)) td))
				(let ((nodetype (node-name (car s))) (value (node-op (car s))))
					(case nodetype
						((begin-term) (get-term-loop (cdr s) (if (= td 0) t (cons (car s) t)) (+ td 1)))
						((end-term)
							(if (= td 1)
								(cons (cdr s) t)
								(get-term-loop (cdr s) (cons (car s) t) (- td 1))
							)
						)
						((const)
							(let ((entry (lookup table value proc)))
								(if entry
									(if (eq? (node-name (cadr s)) 'begin-args)
										(let* ((r1 (get-args (cdr s) (entry-params entry) table proc value)) (s1 (car r1)) (args (cdr r1)))
											(get-term-loop s1 (append args (cons (car s) t)) td)
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
						((val add sub mul div) (get-term-loop (cdr s) (cons (car s) t) td))
						(else
							(fmt "internal error: illegal syntax node" nodetype "in get-term")
							(return '!internal-error)
						)
					)
				)
			)
		)
		; get-condn : [Node],[Entry],symbol -> [Node],[Node]
		(define (get-condn s0 table proc)
			(let get-condn-loop ((s s0) (c '()))
				(if debug.semantic (fmt "get-condn" (node.unpack (car s))))
				(let ((nodetype (node-name (car s))) (value (node-op (car s))))
					(case nodetype
						((begin-cond) (get-condn-loop (cdr s) c))
						((end-cond)
							(cons (cdr s) c))
						((const val eq ne gt lt)
							(get-condn-loop (cdr s) (cons (car s) c))
						)
						((begin-args)
							(let ((entry (lookup table (node-op (car c)) proc)))
								(if entry
									(let* ((r1 (get-args (cdr s) (entry-params entry) table proc (node-op (car c)))) (s1 (car r1)) (args (cdr r1)))
										(get-condn-loop s1 (append args (cons (car s) c)))
									)
									(begin
										(fmt "error: reference to unknown constant" (node-op (car c)))
										(return '!semantic-error)
									)
								)
							)
						)
						((cond-else) (get-condn-loop (cdr s) (cons (car s) c)))
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

		(define preloaded 
			(let semantic-preload ((syntax syntax0) (table '()))
				(if (null? syntax)
					table
					(let ((x (node-name (car syntax))) (y  (node-op (car syntax))))
						(case x
							((begin-def)
								(if (not (lookup table y))
									(let* ((r (get-params (cdr syntax))) (s (car r)) (p (cdr r)))
										(semantic-preload s (insert table y (entry.new y p '() '())))
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
				(fmt "semantic" (if (null? syntax) '() (node.unpack (car syntax))) (map entry.unpack table))
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
												(get-condn s1 table y)
												(cons (find-next (cdr syntax) 'begin-term '()) '())
											)
										)
									)
									(s2 (car r2)) (condn (reverse (cdr r2))) (t2 (insert table y (entry.new y pars '() condn)))
									(r3 (get-term s2 t2 y)) (s3 (car r3)) (body (reverse (cdr r3))) (t3 (insert table y (entry.new y pars body condn)))
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