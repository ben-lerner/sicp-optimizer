(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (val)
               (if trace
                   (begin
                     (display "register ")
                     (display name)
                     (display ": ")
                     (display contents)
                     (display " -> ")
                     (display val)
                     (newline)))
               (set! contents val)))
            ((eq? message 'trace-on) (set! trace true))
            ((eq? message 'trace-off) (set! trace false))
            (else
             (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register val) ((register 'set) val))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0)
        (cycles 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      (set! cycles 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'cycles '= cycles
                     'total-pushes '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'inc) (set! cycles (+ 1 cycles)))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics) (print-statistics))
            (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (in? item L)
  (cond ((null? L) false)
        ((equal? item (car L)) true)
        (else (in? item (cdr L)))))

(define (del-el item L)
  (cond ((null? L) (error "item not in list: " item))
        ((equal? item (car L)) (cdr L))
        (else (cons (car L) (del-el item (cdr L))))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-counter 0)
        (instruction-trace-on false)
        (breakpoints '())
        (cur-label '*unassigned*) ; should be a gensym
        (n-past-label 0))
    (let ((the-ops
           (list (list 'initialize-stack (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Register defined more than once: " name)
            (set! register-table
                  (cons (list name (make-register name)) register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (stack 'inc) ; increment instruction counter
                (set! instruction-counter (+ 1 instruction-counter))
                (set! n-past-label (+ 1 n-past-label))
                (if (not (at-breakpoint cur-label n-past-label))
                    (let ((instruction (car insts)))
                      (if instruction-trace-on
                          (begin (display (instruction-text instruction))
                                 (newline)))
                      ((instruction-execution-proc instruction))
                      (execute)))))))
      (define (set-cur-label label)
        (set! n-past-label 0)
        (set! cur-label label))
      ;; using a linear breakpoint structure is bad, but afaict alists are
      ;; linear anyway. We'd want a map in production.
      (define (set-breakpoint label n)
        (let ((bp (list label n)))
          (if (in? bp breakpoints)
              (error "Breakpoint already exists:" bp)
              (set! breakpoints (cons bp breakpoints))))
        breakpoints)
      (define (clear-breakpoint label n)
        (let ((bp (list label n)))
          (if (in? bp breakpoints)
              (set! breakpoints (del-el bp breakpoints))
              (error "Breakpoint doesn't exist:" bp))))
      (define (at-breakpoint label n)
        (in? (list label n) breakpoints))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'instruction-counter) instruction-counter)
              ((eq? message 'reset-instruction-counter)
               (set! instruction-counter 0))
              ((eq? message 'decrement-instruction-counter)
               (set! instruction-counter (- instruction-counter 1)))
              ((eq? message 'trace-on) (set! instruction-trace-on true))
              ((eq? message 'trace-off) (set! instruction-trace-on false))
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'clear-breakpoint) clear-breakpoint)
              ((eq? message 'clear-all-breakpoints) (set! breakpoints '()))
              ((eq? message 'set-cur-label) set-cur-label)
              ((eq? message 'proceed) (execute))
              (else (error "Unknown request: MACHINE " message))))
      dispatch)))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name val)
  (set-contents! (get-register machine register-name) val)
  'done)
(define (set-breakpoint machine label n) ((machine 'set-breakpoint) label n))
(define (clear-breakpoint machine label n) ((machine 'clear-breakpoint) label n))
(define (clear-all-breakpoints machine) (machine 'clear-all-breakpoints))
(define (proceed-machine machine) (machine 'proceed))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;; assembler

(define (assemble controller-text machine)
  (extract-labels
   controller-text
   (lambda (insts labels)
     (update-insts! insts labels machine)
     insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (let ((insts (cons (make-instruction next-inst) insts)))
             (receive
                 insts
                 (if (symbol? next-inst)
                     (cons (make-label-entry next-inst insts) labels) labels))))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stack ops)))
     insts)))


;; the text is for debugging
;;; a real compiler would use a flag to determine whether to drop it from the binary
(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc) (set-cdr! inst proc))
(define (make-label-entry label-name insts) (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: AVENGERS ASSEMBLE" label-name))))

(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (if (symbol? inst) ; label
      (make-label-noop inst machine labels ops pc)
      (let ((tag (car inst)))
        (cond ((eq? tag 'assign)
               (make-assign inst machine labels ops pc))
              ((eq? tag 'test)
               (make-test inst machine labels ops flag pc))
              ((eq? tag 'branch)
               (make-branch inst machine labels flag pc))
              ((eq? tag 'goto)
               (make-goto inst machine labels pc))
              ((eq? tag 'save)
               (make-save inst machine stack pc))
              ((eq? tag 'restore)
               (make-restore inst machine stack pc))
              ((eq? tag 'perform)
               (make-perform inst machine labels ops pc))
              (else
               (error "Unknown instruction type:" inst))))))

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp value-exp machine operations labels)
               (make-primitive-exp (car value-exp) machine labels))))
      (lambda () ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp condition machine operations labels)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad Robot" inst))))

(define (test-condition test-instruction) (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label
                labels
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Branching to a non-label" inst))))
(define (branch-dest branch-instruction) (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label
                         labels
                         (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register
                       machine
                       (register-exp-reg dest))))
             (lambda () (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO (ah, but I repeat myself)" inst)))))

(define (goto-dest goto-instruction) (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-inst) (cadr stack-inst))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp action machine operations labels)))
          (lambda () (action-proc) (advance-pc pc)))
        (error "you can't do this:" inst))))
(define (perform-action inst) (cdr inst))

(define (make-label-noop inst machine labels operations pc)
  (lambda ()
    (machine 'decrement-instruction-counter)
    ((machine 'set-cur-label) inst)
    (advance-pc pc)))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label
                       labels
                       (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type" exp))))

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine operations labels)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs ;; argument procedures
         (map (lambda (e) (make-arg e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (make-arg exp machine labels) (make-primitive-exp exp machine labels))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))
(define (operation-exp-op exp) (cadar exp))
(define (operation-exp-operands exp) (cdr exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown primitive op:" symbol))))


;;;;;;;;;;
;; test ;;
;;;;;;;;;;

(define (breakpoint-test)
  (let ((machine
         (make-machine
          '(a b)
          (list (list 'inc (lambda (n) (+ n 1))) (list '> >))
          '((assign a (const 0))
            (assign b (const 0))
            start
            (assign a (op inc) (reg a))
            (assign a (op inc) (reg a))
            (assign a (op inc) (reg a))
            (assign a (op inc) (reg a))
            (assign a (op inc) (reg a))
            middle
            (assign b (op inc) (reg b))
            (assign b (op inc) (reg b))
            (assign b (op inc) (reg b))
            (assign b (op inc) (reg b))
            (assign b (op inc) (reg b))
            (test (op >) (reg a) (const 1000))
            (branch (label end))
            (goto (label start))
            end))))

    (define (check a b)
      (define (should-be register n)
        (display "this should be ")
        (display n)
        (display ": ")
        (display (get-register-contents machine register))
        (newline))
      (should-be 'a a)
      (should-be 'b b)
      (newline))

    (define (go) (proceed-machine machine))

    (set-breakpoint machine 'start 3)
    (start machine)
    (check 2 0)
    (go)
    (check 7 5)
    (set-breakpoint machine 'middle 3)
    (go)
    (check 10 7)
    (go)
    (check 12 10)
    (clear-breakpoint machine 'start 3)
    (go)
    (check 15 12)
    (go)
    (check 20 17)
    (clear-all-breakpoints machine)
    (get-register-contents machine 'a)))

; (breakpoint-test)
