(load "optimizer.scm")

(define (print str)
  (display str)
  (newline))

(newline)
(print "***********************************")
(print "      Optimization test suite      ")
(print "***********************************")
(newline)

(define (get-registers-test)
  (let ((reg
         (get-registers
          '((test (op =) (reg n) (const 1))
            (branch (label two))
            (assign n (const 1))
            (goto (label branch-done))
            two
            (assign n (const 2))
            branch-done
            (test (op <) (reg n) (const 3))
            (branch (label lt))
            (assign val (const "<3"))
            (goto (label done))
            (assign val (const "</3"))
            done))))
    (or (equal? reg '(n val))
        (equal? reg '(val n)))))

(define cleanup-test-1
  '((goto (label done))      ; 1
    (assign a (const 1))     ; 2
    (assign b (const 2))     ; 3
    (assign c (const 3))     ; 4
    done                     ; 5
    (goto (label very-done)) ; 6
    (assign d (const 4))     ; 7
    (assign e (const 5))     ; 8
    (assign f (const 6))     ; 9
    very-done                ; 10
    (assign g (const 7))))   ; 11

(define cleanup-test-2
  '((goto (label done))
    done
    (goto (label very-done))
    very-done
    (assign g (const 7))))

(define cleanup-test-3
  '(done
    very-done
    (assign g (const 7))))

(define (label-path-test)
  (equal? (make-label-paths cleanup-test-1)
          '((done 4 1) (very-done 9 6))))

(define (goto-cleanup-test)
  (equal? (goto-cleanup cleanup-test-2)
          cleanup-test-3))

(define (unreachable-code-test)
  (equal? (unreachable-code-cleanup cleanup-test-1)
          cleanup-test-2))

(define (label-assignment-test)
  (and
   (label-assignment? '(assign a (label foo)))
   (not (label-assignment? '(assign b (const 3))))))

(define (noop-static-goto-test)
  (and
   (noop-static-goto '(goto (label a)) 'a)
   (noop-static-goto '(branch (label a)) 'a)
   (not (noop-static-goto '(goto (reg a)) 'a))
   (not (noop-static-goto '(goto (label a)) 'b))
   (not (noop-static-goto '(goto (label a)) '(assign a (const 4))))))

(define integration-label-cleanup-data
  '(((foo
      bar
      cat)
     .
     ())

    ((foo
      (goto (label cat))
      cat
      bar)
     .
     ())

    (((assign a (label foo))
      (goto (reg a))
      (assign b (const 1))
      foo)
     .
     ((assign a (label foo))
      (goto (reg a))
      (assign b (const 1))
      foo))
    ))

(define label-cleanup-data
  '((((goto (label cat))
      bar
      cat)
     .
     ((goto (label cat))
      cat))

    (((assign a (label cat))
      (goto (reg a))
      bar
      cat)
     .
     ((assign a (label cat))
      (goto (reg a))
      cat))

    (((assign a (label foo))
      (goto (reg a))
      foo)
     .
     ((assign a (label foo))
      (goto (reg a))
      foo))))

(define (label-cleanup-test)
  (run-tests
   label-cleanup
   label-cleanup-data))

(define fuse-label-data
  '(((foo
      bar
      cat)
     .
     (foo))

    (((goto (label bar))
      foo
      bar
      cat)
     .
     ((goto (label foo))
      foo))))

(define (label-fuse-test)
  (run-tests
   fuse-consecutive-labels
   fuse-label-data))

(define branch-test-cleanup-data
  '((((test (op false?) (reg val)))
     .
     ())

    (((test (op false?) (reg val))
      (branch (label test-branch)))
     .
     ((test (op false?) (reg val))
      (branch (label test-branch))))

    (((test (op true?) (reg val))
      (test (op false?) (reg val))
      (branch (label test-branch)))
     .
     ((test (op false?) (reg val))
      (branch (label test-branch))))

    (((test (op true?) (reg val))
      (assign val (op -) (reg arg1) (reg arg2))
      (test (op false?) (reg val))
      (branch (label test-branch)))
     .
     ((assign val (op -) (reg arg1) (reg arg2))
      (test (op false?) (reg val))
      (branch (label test-branch))))))

(define (branch-test-cleanup-test)
  (run-tests
   branch-test-cleanup
   branch-test-cleanup-data))

(define inline-constant-data
  '((((assign arg1 (const 3))
      (assign arg2 (op +) (reg arg1) (const 2)))
     .
     ((assign arg1 (const 3))
      (assign arg2 (op +) (const 3) (const 2))))))

(define (inline-constants-test)
  (run-tests
   identity
   inline-constant-data))

(define drop-unread-register-assigments-data
  '((((assign arg1 (const 3))
      (assign arg2 (op +) (const 3) (const 2)))
     .
     ((assign arg2 (op +) (const 3) (const 2))))))

(define (drop-unread-register-assigments-test)
  (run-tests
   identity
   drop-unread-register-assigments-data))

(define constant-folding-data
  '((((assign val (op -) (const 3) (const 2)))
     .
     ((assign val (const 1))))

    (((assign val (op <) (const 2) (const 3)))
     .
     ((assign val (const #t))))

    (((test (op =) (const 0) (const 1)))
     .
     ((test (op true?) (const #f))))

    (((assign foo (op =) (reg a) (reg b)))
     .
     ((assign foo (op =) (reg a) (reg b))))

    ((foo)
     .
     (foo))))

(define (constant-folding-test)
  (run-tests
   fold-constants
   constant-folding-data))

;; todo: where does type inferencing do anything?
(define (type-inference-test)
  (run-tests
   identity
   type-inference-data))

(define value-inference-data
  '((((test (op >) (reg arg1) (reg arg2))
      (branch (label foo))
      (assign val (const 2))
      (goto (label bar))
      foo
      (assign val (const 1))
      bar
      (assign val (op >) (reg val) (const 0)))
     .
     (assign val (const true)))

    (((test (const false))
      (branch (label test-branch))
      (assign val (const 1)))
     .
     ((assign val (const 1))))

    (((test (const true))
      (branch (label test-branch))
      (assign val (const 1)))
     .
     ((goto (label test-branch))
      (assign val (const 1))))))

(define (value-inference-test)
  (run-tests
   identity
   value-inference-data))

;; integration tests
;; (define (map-fold-test)
;;   (equal?
;;    '((test (op =) (reg n) (const 1))
;;      (branch (label two))
;;      (assign n (const 1))
;;      (goto (label branch-done))
;;      two
;;      (assign n (const 2))
;;      branch-done
;;      (test (op <) (reg n) (const 3))
;;      (branch (label lt))
;;      (assign val (const "<3"))
;;      (goto (label done))
;;      (assign val (const "</3"))
;;      done)
;;    '((assign val (const "<3")))))

()


(define (save-restore-test)
  (run-tests
   identity
   save-restore-data))

;;;;; Test utilities

(define (print-test input expected-output output)
  (print "\nInput:")
  (print input)
  (print "\nExpected:")
  (print expected-output)
  (print "\nGot:")
  (print output))

(define (run-tests test-fn tests)
  (define (pass? test)
    (let* ((input (car test))
           (output (test-fn input))
           (expected-output (cdr test)))
      (if (equal? output expected-output)
          #t
          (begin
            (print-test input expected-output output)
            #f))))
  (all (map pass? tests)))

(define (run tests)
  (define (run-helper pass total tests)
    (if (null? tests)
        (print (string-append
                "\n"(string pass) "/" (string total) " tests passed\n"))
        (let ((test-name (caar tests))
              (test-passed? ((cadar tests))))
          (print (string (if test-passed? "pass" "fail")
                         ": " test-name))
          (run-helper (+ pass (if test-passed? 1 0))
                        (inc total)
                        (cdr tests)))))
  (run-helper 0 0 tests))

(run
 `(;; unit tests
   ("get registers" ,get-registers-test)
   ("unreachable code" ,unreachable-code-test)
   ("goto cleanup" ,goto-cleanup-test)
   ("noop static goto" ,noop-static-goto-test)
   ("label cleanup" ,label-cleanup-test)
   ("label fuse" ,label-fuse-test)
   ("branch cleanup" ,branch-test-cleanup-test)
;   ("inline constants" ,inline-constants-test)
;;   ("drop unread register assignments" ,drop-unread-register-assigments-test)
   ("constant folding" ,constant-folding-test)
;   ("type inferencing" ,type-inference-test)
;   ("value inferencing" ,value-inference-test)
;;    ;; integration tests
;;    ;; ("map fold" ,map-fold-test)
;;    ;; integration-label-cleanup
   ))
