(define factorial-compiled  ;; from print-factorial in 5-compiler.scm
  '((assign val (op make-compiled-procedure) (label entry2) (reg env))
    (goto (label after-lambda1))
    entry2
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
    (assign arg1 (op lexical-address-lookup) (const (0 . 0)) (reg env))
    (assign arg2 (const 1))
    (assign val (op =) (reg arg1) (reg arg2))
    (test (op false?) (reg val))
    (branch (label false-branch4))
    true-branch5
    (assign val (const 1))
    (goto (reg continue))
    false-branch4
    (save continue)
    (save env)
    (assign proc (op lookup-global-variable-value) (const factorial))
    (assign arg1 (op lexical-address-lookup) (const (0 . 0)) (reg env))
    (assign arg2 (const 1))
    (assign val (op -) (reg arg1) (reg arg2))
    (assign argl (op list) (reg val))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch8))
    (test (op compiled-procedure?) (reg proc))
    (branch (label compiled-branch7))
    (save continue)
    (goto (reg compapp))
    compiled-branch7
    (assign continue (label proc-return9))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    proc-return9
    (assign arg1 (reg val))
    (goto (label after-call6))
    primitive-branch8
    (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call6
    (restore env)
    (assign arg2 (op lexical-address-lookup) (const (0 . 0)) (reg env))
    (restore continue)
    (assign val (op *) (reg arg1) (reg arg2))
    (goto (reg continue))
    after-if3
    after-lambda1
    (perform (op define-variable!) (const factorial) (reg val) (reg env))
    (assign val (const ok))))

(define factorial-hand-tuned  ;; from 5-factorial-measurements
  '((assign val (const 1))
    fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label fact-done))
    (assign val (op *) (reg val) (reg n))
    (assign n (op -) (reg n) (const 1))
    (goto (label fact-loop))
    fact-done))
