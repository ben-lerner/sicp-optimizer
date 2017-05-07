(load "optimizer.scm")

(define (print str)
  (display str)
  (newline))

(newline)
(print "****************************************")
(print "Optimization test suite")
(print "****************************************")
(newline)

(define (get-registers-test)
  (equal?
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
     done)
   )
   '(n val)
   ))

(define test-code-1
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

(define test-code-2
  '((goto (label done))
    done
    (goto (label very-done))
    very-done
    (assign g (const 7))))

(define (label-path-test)
  (equal? (make-label-paths test-code-1)
          '((done 4 1) (very-done 9 6))))

(define (goto-cleanup-test)
  (equal? (goto-cleanup test-code-2)
          '(done
            very-done
            (assign g (const 7)))))

(define (unreachable-code-test)
  (equal? (unreachable-code-cleanup test-code-1)
          test-code-2))

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

(define (label-cleanup-test)
  (let
      ((tests ; input-output pairs
        '(((foo
            bar
            cat)
           .
           ())

          ((foo
            (goto (label cat))
            bar
            cat)
           .
           ((goto (label cat))
            cat))
          
          ((foo
            (goto (label cat))
            cat
            bar)
           .
           ((goto (label cat))
            cat))
          
          (((assign a (label foo))
            (goto (reg a))
            foo)
           .
           ((assign a (label foo))
            (goto (reg a))
            foo))
          
          (((assign a (label foo))
            (goto (reg a))
            bar
            foo)
           .
           ((assign a (label foo))
            (goto (reg a))
            foo))
          
          (((assign a (label foo))
            (goto (reg a))
            (assign b (const 1))
            foo)
           .
           ((assign a (label foo))
            (goto (reg a))
            (assign b (const 1))
            foo))
          )))
    (and
     (map (lambda (x) (equal? (label-cleanup (car x)) (cdr x)))
          tests)))
  
  
  )

(define (branch-test-cleanup-test)
(let
      ((tests ; input-output pairs
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
            (assig val (op -) (reg arg1) (reg arg2))
            (test (op false?) (reg val))
            (branch (label test-branch)))
           .
           ((assig val (op -) (reg arg1) (reg arg2))
            (test (op false?) (reg val))
            (branch (label test-branch))))
          )))
    (and
     (map (lambda (x) (equal? (branch-test-cleanup (car x)) (cdr x)))
          tests)))  )

;; (define (map-fold-test)
;;   ;; todo: insert correct fn's
;;   (equal?                               ; check the contents of two lists
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


(define (run tests)
  (define (run-helper pass total tests)
    (if (null? tests)
        (print (string-append "Done: "(string pass) "/" (string total) " tests passed"))
        (let ((test-name (caar tests))
              (test (cadar tests)))
          (if (test)
              (run-helper (+ 1 pass) (+ 1 total) (cdr tests))
              (begin
                (if (= pass total) ; first failed test
                    (newline))
                (print (string-append "failed \"" test-name "\""))
                (run-helper pass (+ 1 total) (cdr tests)))))))
  (run-helper 0 0 tests))

;; todo: print failed results

(run
 `(("get registers test" ,get-registers-test)
   ("unreachable code test" ,unreachable-code-test)
   ("goto cleanup test" ,goto-cleanup-test)
   ("noop static goto test" ,noop-static-goto-test)
   ("label cleanup test" ,label-cleanup-test)
   ("branch test cleanup test", branch-test-cleanup-test)
   ))