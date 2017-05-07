; post exercise 5-32

(load "4-evaluator-utils.scm")
(load "5-19.scm") ;; register machine, includes debugging features
(load "5-lexical-utils.scm")

;; chapter footnote functions
(define (get-global-environment) the-global-environment)
(define (empty-arglist) '())
(define (last-operand? ops) (null? (cdr ops)))
(define (adjoin-arg arg arglist) (append arglist (list arg)))


;; 5-48
(define (compile-and-run expression machine)
  (assemble (statements
             (compile expression 'val 'return (make-new-c-env)))
            machine))

(define (compile-and-run? exp)
  (and (application? exp)
       (eq? (car exp) 'compile-and-run)))

;;;;;; interpreter

(define eceval-controller
  '((assign compapp (label compound-apply)) ; let compiler jump to compound-apply
    (branch (label external-entry)) ; branches if flag is set
    read-eval-print-loop
    (perform (op initialize-stack))
    (perform (op prompt-for-input) (const ";;EC-Eval input: "))
    (assign exp (op read))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (label eval-dispatch))

    print-result
    (perform (op print-stack-statistics)) ;; for evaluating performance
    (perform (op announce-output) (const ";;EC-Eval value: "))
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))

    external-entry
    (perform (op initialize-stack))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (reg val))
    
    compile-and-run
    (assign continue (label print-result))
    (assign exp (op operands) (reg exp)) ;; drop compile-and-run
    (assign exp (op first-operand) (reg exp)) ;; extract text
    (assign exp (op text-of-quotation) (reg exp)) ;; drop quote
    ;; note: no error checking. Error checking would be very good here.
    (assign val (op compile-and-run) (reg exp))
    (goto (reg val))
    
    eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))
    (test (op definition?) (reg exp))
    (branch (label ev-definition))
    (test (op if?) (reg exp))
    (branch (label ev-if))
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))
    (test (op compile-and-run?) (reg exp))
    (branch (label compile-and-run))
    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-expression-type))
    
    ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))
    
    ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (goto (reg continue))
    
    ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))
    
    ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
    (goto (reg continue))

    ev-application
    (save continue)
    (assign unev (op operands) (reg exp))
    (assign exp (op operator) (reg exp))
    (test (op variable?) (reg exp))
    (goto (label ev-appl-symbol))
    (save env)
    (save unev)
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))

    ev-appl-did-operator
    (restore unev) ; the operands
    (restore env)
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)
    (goto (label ev-appl-operand-loop))
    
    ev-appl-symbol
    (assign argl (op empty-arglist))
    (assign proc (op lookup-variable-value) (reg exp) (reg env))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)
    
    ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev)) ; evlis tail recrusion
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))
    
    ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))

    ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))

    ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))

    apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (test (op compiled-procedure?) (reg proc))
    (branch (label compiled-apply))
    (perform (op display) (reg proc))
    (goto (label unknown-procedure-type))

    primitive-apply
    (assign val
            (op apply-primitive-procedure)
            (reg proc)
            (reg argl))
    (restore continue)
    (goto (reg continue))

    compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment)
            (reg unev) (reg argl) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))

    compiled-apply
    (restore continue)
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    
    ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))

    ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))

    ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))

    ev-sequence-last-exp
    (restore continue)
    (goto (label eval-dispatch))

    ev-if
    (save exp) ; save expression for later
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch)) ; evaluate the predicate

    ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))

    ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))

    ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))

    ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev) ; save for later
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-1))
    (goto (label eval-dispatch)) ; evaluate the assignment value

    ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

    ev-definition
    (assign unev (op definition-variable) (reg exp))
    (save unev) ; save for later
    (assign exp (op definition-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-definition-1))
    (goto (label eval-dispatch)) ; evaluate the assignment value

    ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))
    
    ;; errors
    unknown-expression-type
    (assign val (const unknown-expression-type-error))
    (goto (label signal-error))
    
    unknown-procedure-type
    (restore continue) ; clean up stack (from apply-dispatch)
    (assign val (const unknown-procedure-type-error))
    (goto (label signal-error))

    signal-error
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))))

(define eceval-operations
  `((self-evaluating? ,self-evaluating?)
    (variable? ,variable?)
    (quoted? ,quoted?)
    (text-of-quotation ,text-of-quotation)
    (assignment? ,assignment?)
    (assignment-variable ,assignment-variable)
    (assignment-value ,assignment-value)
    (definition? ,definition?)
    (definition-variable ,definition-variable)
    (definition-value ,definition-value)
    (lambda? ,lambda?)
    (lambda-parameters ,lambda-parameters)
    (lambda-body ,lambda-body)
    (if? ,if?)
    (if-predicate ,if-predicate)
    (if-consequent ,if-consequent)
    (if-alternative ,if-alternative)
    (application? ,application?)
    (operator ,operator)
    (operands ,operands)
    (no-operands? ,no-operands?)
    (first-operand ,first-operand)
    (rest-operands ,rest-operands)
    (empty-arglist ,empty-arglist)
    (adjoin-arg ,adjoin-arg)
    (last-operand? ,last-operand?)
    (get-global-environment ,get-global-environment)
    (user-print ,user-print)
    (extend-environment ,extend-environment)
    (define-variable! ,define-variable!)
    (apply-primitive-procedure ,apply-primitive-procedure)
    (prompt-for-input ,prompt-for-input)
    (announce-output ,announce-output)
    (read ,read)
    (begin? ,begin?)
    (begin-actions ,begin-actions)
    (last-exp? ,last-exp?)
    (first-exp ,first-exp)
    (rest-exps ,rest-exps)
    (make-procedure ,make-procedure)
    (primitive-procedure? ,primitive-procedure?)
    (make-compiled-procedure ,make-compiled-procedure)
    (compound-procedure? ,compound-procedure?)
    (compiled-procedure? ,compiled-procedure?)
    (compiled-procedure-entry ,compiled-procedure-entry)
    (compiled-procedure-env ,compiled-procedure-env)
    (procedure-parameters ,procedure-parameters)
    (procedure-body ,procedure-body)
    (procedure-environment ,procedure-environment)
    (true? ,true?)
    (false? ,false?)
    (display ,display)
    (newline ,newline)
    (list ,list)
    (cons ,cons)
    (= ,=)
    (+ ,+)
    (* ,*)
    (- ,-)
    (lexical-address-lookup ,lexical-address-lookup)
    (lexical-address-set! ,lexical-address-set!)
    (set-variable-value! ,set-variable-value!)
    (lookup-variable-value ,lookup-variable-value)
    (set-global-variable-value! ,set-global-variable-value!)
    (lookup-global-variable-value ,lookup-global-variable-value)
    (compile-and-run ,(lambda (exp) (compile-and-run exp eceval)))
    (compile-and-run? ,compile-and-run?)
    ))

(define eceval
  (make-machine
   '(exp env val proc argl arg1 arg2 continue unev compapp)
   eceval-operations
   eceval-controller))

; (eceval 'trace-on)

(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))
