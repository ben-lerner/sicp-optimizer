;; utils needed for the evaluator + for ch 5's interpreter

;; section 4.1
(define apply-in-underlying-scheme apply) ;; save reference to underlying apply

(define (prompt-for-input string)
  (newline) (newline) (display string))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((compiled-procedure? object) (display '<compiled-procedure>))
        (else (display object))))

(define (apply-primitive-procedure procedure args)
  (apply-in-underlying-scheme (primitive-implementation procedure) args))

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;; syntax
(define (self-evaluating? exp)
  (cond ((number? exp) true)
		((string? exp) true)
		(else false)))

(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
	  (eq? (car exp) tag)
	  false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (or (tagged-list? exp 'def)
      (tagged-list? exp 'define)))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
	  (cadr exp)
	  (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
	  (caddr exp)
	  (make-lambda (cdadr exp) ; formal parameters
				   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
	  (cadddr exp)
	  'false))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (make-begin seq) (cons 'begin seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))))

(define (expand-clauses clauses)
  (if (null? clauses)
	  'false ; no else clause
	  (let ((first (car clauses))
			(rest (cdr clauses)))
		(if (cond-else-clause? first)
			(if (null? rest)
				(sequence->exp (cond-actions first))
				(error "ELSE clause isn't last -- COND->IF" clauses))
			(make-if (cond-predicate first)
					 (sequence->exp (cond-actions first))
					 (expand-clauses rest))))))

;; environments and frames

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame vars vals)
  (cons vars vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
	  (cons (make-frame vars vals) base-env)
	  (error "Len of vars and vals doesn't match" vars vals)))

(define (lookup-variable-value var env)
  (define (env-loop env)
	(define (scan vars vals)
	  (cond ((null? vars)
			 (env-loop (enclosing-environment env)))
			((eq? var (car vars))
			 (car vals))
			(else (scan (cdr vars) (cdr vals)))))
	(if (eq? env the-empty-environment)
		(error "Unbound variable" var)
		(let ((frame (first-frame env)))
		  (scan (frame-variables frame)
				(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
		  (frame-values frame))))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
		(list 'cons cons)
		(list 'null? null?)
        (list '+ +)
        (list '= =)
        (list '- -)
        (list '* *)
        (list '> >)
        (list '< <)
        (list 'eq? eq?)
        (list 'apply apply-primitive-procedure)
        (list 'list list)
        (list 'display display)
        (list 'newline newline)
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list 'number? number?)
        (list 'string? string?)
        (list 'symbol? symbol?)
        (list 'pair? pair?)
        (list 'error error)
))

(define (primitive-procedure-names) (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
	initial-env))

(define the-global-environment (setup-environment))

;; compiled procedure machine ops, from **--the future--** (ch5)
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

;; let expressions
(define (let? exp) (tagged-list? exp 'let))

(define (make-let vars vals body)
  (append (list 'let (zip vars vals)) body))

(define (let-to-lambda exp)
  (let ((bindings (cadr exp))
        (body (cddr exp)))
    (let ((vars (map car bindings))
          (vals (map cadr bindings)))
      `((lambda ,vars
          ,@body)
        ,@vals))))

;; scan-out-defines, from 4-16, for compile-time env
(define (make-scanned-exp vars body)
  (make-let
   vars (map (lambda (_) ''*unassigned*) vars) body))

(define (scan-out body) ;; returns '(vars body)
	(if (null? body)
		(list '() body)
		(let ((first (car body))
			  (rest (scan-out (cdr body))))
		  (let ((rest-vars (car rest))
				(rest-body (cadr rest)))
			(if (definition? first)
				(list (cons (definition-variable first) rest-vars)
					  (cons (list 'set!
								  (definition-variable first)
								  (definition-value first))
							rest-body))
				(list rest-vars (cons first rest-body)))))))

(define (scan-out-defines exp)
  (let ((scanned (scan-out exp)))
    (let ((vars (car scanned))
          (body (cdr scanned)))
      (if (null? vars)
          (car body)
          (make-scanned-exp vars body)))))

(define (test-scan-one)
  (let ((exp '(lambda (a b) (+ a b))))
    (scan-out-defines exp)))

(define (test-scan-two)
  (let ((exp '(lambda (a b) (define c (+ a b)) (+ a b c))))
    (scan-out-defines exp)))

(define (set-global-variable-value! var val)
  (set-variable-value! var val (get-global-environment)))

(define (lookup-global-variable-value var)
  (lookup-variable-value var (get-global-environment)))
