;;; fcl code generator - C language

; generate-c : [Entry] -> string
(define (generate-c table)
    (define (push stack . value)
        (if (null? (car value))
            stack
            (cons (car value) (push stack (cdr value)))

        )
    )

    ; gen-expr : [Node] -> string
    (define (gen-expr exp0 table)
        (let gen-loop ((exp exp0) (s '()) (ad 0) (td 0))
            (if debug.generate (fmt "gen-expr" (map node.unpack exp) "; " s ad td))
            (if (null? exp)
                (if (null? s) s (car s))
                (let ((type (node-name (car exp))) (value (node-op (car exp))))
                    (case type
                        ((cond-else end-cond) (if (null? s) s (car s)))
                        ((begin-term)
                            (gen-loop (cdr exp) s ad (+ td 1))
                        )
                        ((end-term)
                            (gen-loop 
                                (cdr exp)
                                (if (= td 1)
                                    (cond 
                                        ((= ad 0) (string-append "(" (car s) ")"))
                                        ((eq? (node-name (cadr exp)) 'end-args) (push (cddr s) (string-append (cadr s) (car s))))
                                        (else (push (cddr s) (string-append (cadr s) (car s) ",")))
                                    )
                                    ;(push (cdr s) (string-append "(" (car s) ")"))
                                    s
                                )
                                ad 
                                (- td 1)
                            )
                        )
                        ((val) (gen-loop (cdr exp) (push s (number->string value)) ad td))
                        ((const) (gen-loop (cdr exp) (push s (symbol->string value)) ad td))
                        ((begin-args)
                            (if (eq? (node-name (cadr exp)) 'end-args)
                                (gen-loop (cddr exp) (push (cdr s) (if (lookup table (string->symbol (car s))) (string-append (car s) "()") (car s))) ad td)
                                (gen-loop (cdr exp) (push (cdr s) (string-append (car s) "(")) (+ ad 1) 0)
                            )
                        )
                        ((end-args)
                            (gen-loop (cdr exp)
                                (if (< ad 2)
                                    (push (cdr s) (string-append (car s) ")"))
                                    (push (cddr s) (string-append (cadr s) (car s) ")"))
                                )
                                (- ad 1) td
                            )
                        )
                        ((add sub mul div lt gt ne eq)
                            (let    ((l (cadr s)) 
                                    (r (car s)) 
                                    (op (case type
                                        ((add) "+")
                                        ((sub) "-")
                                        ((mul) "*")
                                        ((div) "/")
                                        ((eq) "==")
                                        ((ne) "!=")
                                        ((gt) ">")
                                        ((lt) "<")
                                        )
                                    )
                                    )
                                (gen-loop (cdr exp) (push (cddr s) (string-append l op r)) ad td)
                            )
                        )
                        (else
                            (fmt "internal error: invalid node type" type "in gen-expr")
                            '!internal-error
                        )
                    )
                )
            )
        )
    )

    (let generate-loop ((i 0))
        (if (< i (length table))
            (let ((e (list-ref table i)))
                (if debug.generate (fmt "generate" (entry-name e) (entry-condn e)))
                (string-append "int " (symbol->string (entry-name e)) "("
                    (if (null? (entry-params e))
                        "void"
                        (let param-loop ((p (entry-params e)))
                            (cond 
                                ((null? p) "")
                                ((null? (cdr p)) (string-append "int " (symbol->string (car p))))
                                (else (string-append "int " (symbol->string (car p)) "," (param-loop (cdr p))))
                            )
                        )
                    )
                    ") {\n"
                    (if (null? (entry-condn e))
                        ""
                        (string-append
                            "  if ("
                            (gen-expr (entry-condn e) table)
                            ")\n"
                        )
                    )
                    "    return " (gen-expr (entry-body e) table) ";\n"
                    (let ((ce (find-next (entry-condn e) 'cond-else 'end-cond)))
                        (if ce
                            (string-append
                                "  else return "
                                (gen-expr (cdr ce) table)
                                ";\n"
                            )
                            ""
                        )
                    )
                    "}\n"
                    (generate-loop (+ i 1))
                )
            )
            ""
        )
    )
)