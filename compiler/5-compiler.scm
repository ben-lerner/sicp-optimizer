(load "5-interpreter.scm")

;; labels

(define debug false)
(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))

(define all-regs '(env proc val argl arg1 arg2 continue))

;; compiler

(define (print text)
  (if debug
      (begin (display text) (newline))))

(define (compile exp target linkage c-env)
  (cond ((self-evaluating? exp)
         (begin (print "self-eval")
                (compile-self-evaluating exp target linkage)))
        ((quoted? exp)
         (begin (print "quoted")
                (compile-quoted exp target linkage)))
        ((variable? exp)
         (begin (print "variable")
                (print exp)
                (compile-variable exp target linkage c-env)))
        ((assignment? exp)
         (begin (print "assignment")
                (compile-assignment exp target linkage c-env)))
        ((definition? exp)
         (begin (print "definition")
                (compile-definition exp target linkage c-env)))
        ((if? exp)
         (begin (print "if")
                (compile-if exp target linkage c-env)))
        ((let? exp)
         (begin (print "let")
                (compile-application (let-to-lambda exp) target linkage c-env)))
        ((lambda? exp)
         (begin (print "lambda")
                 (let ((scanned (scan-out-defines exp)))
                   ((if (lambda? scanned) compile-lambda compile) scanned target linkage c-env))))
        ((begin? exp)
         (begin (print "begin")
                (compile-sequence
                 (begin-actions exp) target linkage c-env)))
        ((cond? exp)
         (begin (print "cond")
                (compile (cond->if exp) target linkage c-env)))
        ((open-coded? exp c-env) ;; from 5-38
         (begin (print "open-coded")
                (compile-open-coded exp target linkage c-env)))
        ((application? exp)
         (begin (print "application")
                (compile-application exp target linkage c-env)))
        (else
         (error "Unknown expression type: COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
                                    '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
                                    `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '() (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage c-env)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '(env) (list target)
    (let ((address (find-variable exp c-env)))
      (print "finding var")
      (print exp)
      (print address)
      (if (eq? address 'not-found)
          ;; var is in global env
          `((assign ,target
                    (op lookup-global-variable-value)
                    (const ,exp)))
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,address)
                    (reg env))))))))

(define (compile-assignment exp target linkage c-env)
  (let ((var (assignment-variable exp))
        (get-value-code (compile (assignment-value exp) 'val 'next c-env)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       (let ((address (find-variable var c-env)))
         (if (eq? address 'not-found)
             ;; var is in global env 
             `((perform (op set-global-variable-value!)
                        (const ,var)
                        (reg val))
               (assign ,target (const ok)))
             ;; var is in lexical env
             `((perform (op lexical-address-set!)
                       (reg val)
                       (const ,address)
                       (reg env))))))))))

(define (compile-definition exp target linkage c-env)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next c-env)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op define-variable!)
                  (const , var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-if exp target linkage c-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next c-env))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage c-env))
            (a-code
             (compile (if-alternative exp) target linkage c-env)))
        (preserving
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence
           '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

(define (compile-sequence seq target linkage c-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage c-env)
      (preserving
       '(env continue)
       (compile (first-exp seq) target 'next c-env)
       (compile-sequence (rest-exps seq) target linkage c-env))))


(define (compile-lambda exp target linkage c-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry c-env))
       after-lambda))))

(define (compile-lambda-body exp proc-entry c-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl) '(env)
      `(,proc-entry
        (assign env
                (op compiled-procedure-env)
                (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return (extend-c-env c-env formals)))))

(define (compile-application exp target linkage c-env)
  (let ((proc-code (compile (operator exp) 'proc 'next c-env))
        (operand-codes
         (map (lambda (operand)
                (compile operand 'val 'next c-env))
              (operands exp))))
    (preserving
     '(env continue)
     proc-code
     (preserving
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage c-env)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl) '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val) '(argl) '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving
          '(argl)
          (car operand-codes)
          (make-instruction-sequence
           '(val argl) '(argl)
           '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving
         '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage c-env)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (make-instruction-sequence
        '(proc) '()
        `((test (op compiled-procedure?) (reg proc))
          (branch (label ,compiled-branch))
          (save continue) ; compiled-apply expects this
          (goto (reg compapp))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage c-env))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl) (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))


(define (compile-proc-appl target linkage c-env)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry) (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          ;; this case makes the compiler tail-recursive: it's the usual case,
          ;; and nothing is stored on the stack
          '((assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage, target not val: COMPILE" target))))


;; combining instruction sequences
(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union
      (registers-needed seq1)
      (list-difference (registers-needed seq2)
                       (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences
         (car seqs)
         (append-seq-list (cdr seqs)))))
  
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  ;; elements in s1 that aren't in s2
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
           (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1) (registers-needed seq2))
   (list-union (registers-modified seq1) (registers-modified seq2))
   (append (statements seq1) (statements seq2))))


;; from 5-38
(define open-coded-primitives '(= * - +))

(define (spread-arguments op-list c-env)
  (if (not (= 2 (length op-list)))
      (error "op-list has length " (length op-list))
      (let ((arg1-code (compile (car op-list) 'arg1 'next c-env))
            (arg2-code (compile (cadr op-list) 'arg2 'next c-env)))
        (preserving
         '(env)
         arg1-code
         (preserve-around arg2-code 'arg1)))))

(define (preserve-around seq reg)
  (if (not (modifies-register? seq reg))
      seq
      (make-instruction-sequence
       (list-union (registers-needed seq) (list reg))
       (list-difference (registers-modified seq) (list reg))
       (append `((save ,reg))
               (statements seq)
               `((restore ,reg))))))

(define (application-of? op exp)
  (and (application? exp) (eq? op (car exp))))

(define (open-coded? exp c-env)
  (if (application? exp)
      (let ((proc (car exp)))
        (and (memq proc open-coded-primitives)
             (eq? (find-variable proc c-env) 'not-found)))
      false))

(define (compile-two-open-coded exp target linkage c-env)
  (preserving
   '(env continue)
   (spread-arguments (operands exp) c-env)
   (compile-open-proc (car exp) target linkage)))

(define (compile-open-proc proc target linkage)
  (end-with-linkage
    linkage
    (make-instruction-sequence
     '(arg1 arg2)
     (list target)
     `((assign ,target (op ,proc) (reg arg1) (reg arg2))))))

(define (compile-open-coded exp target linkage c-env)
  (if (= 2 (length (operands exp)))
      (compile-two-open-coded exp target linkage c-env)
      (compile (unfold exp) target linkage c-env)))

(define (unfold exp)
  (let ((op (car exp))
        (args (operands exp)))
    (cond ((eq? op '=) (unfold-= args))
          ((eq? op '-) (unfold-minus args))
          (else (unfold-other op args)))))

(define (unfold-= args)
  (if (= 2 (length args))
      (cons '= args)
      (let ((first-arg (car args))
            (second-arg (cadr args))
            (rest-args (cdr args)))
        `(if (= ,first-arg ,second-arg)
             ,(unfold-= rest-args)
             'false))))

(define (unfold-minus args)
  (list '- (car args) (unfold-other '+ (cdr args))))

(define (unfold-other op args)
  (if (= 2 (length args))
      (cons op args)
      (list op (car args) (unfold-other op (cdr args)))))

;;; compile & go

(define (compile-and-go expression)
  (let ((instructions
         (assemble (statements
                    (compile expression 'val 'return (make-new-c-env)))
                   eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))

(define (test-compile exp)
  (set! label-counter 0)
  (pretty-print
   (statements (compile exp 'val 'next (make-new-c-env)))))

(define fact-text
  '(def (factorial n)
         (if (= n 1)
             1
             (* (factorial (- n 1)) n))))

(define (print-factorial) (test-compile fact-text))
