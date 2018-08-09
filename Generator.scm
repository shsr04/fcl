	;;; could be useful for code generator:
	;((val) (semantic (cdr syntax) table (push stack y)))
	;((add sub mul div)
	;	(semantic (cdr syntax)
	;		(let ((t1 (pop (pop table '$op1) '$op2)))
	;			(push t1
	;				(string-append
	;					(case x ((add) "(+ ") ((sub) "(- ") ((mul ) "(* ") ((div) "(/ "))
	;					(lookup t1 '$op1) " " (lookup t1 '$op2) ")"
	;				)
	;			)
	;		)
	;	)
	;)