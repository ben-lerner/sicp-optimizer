(load "optimizer.scm")

(define (print str)
  (display str)
  (newline))

(newline)
(print "***************************************")
(print "        Optimization test suite        ")
(print "***************************************")
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

(define label-cleanup-test-data
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
    ))

(define branch-test-cleanup-test-data
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
      (branch (label test-branch))))
     ))

(define (label-cleanup-test)
  (run-tests
   label-cleanup
   label-cleanup-test-data))

(define (branch-test-cleanup-test)
  (run-tests
   branch-test-cleanup
   branch-test-cleanup-test-data))

(define (run-tests test-fn tests)
  (all (test-results test-fn tests)))

(define (test-results test-fn tests)
  (map
    (lambda (x) (equal? (test-fn (car x)) (cdr x)))
    tests))

;; (define (map-fold-test)
;;   ;; todo: insert correct fn's
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


;; todo:
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
   ;("map fold test" ,map-fold-test)
   ))
