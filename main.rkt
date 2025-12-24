#lang racket

(require racket/system racket/string)

(struct test (input expecteds))

(struct test-defns test (definitions resets))

(struct test-seq (tests resets))

(define (test-do repl test on-pass on-fail)
  (if (null? (test-expecteds test))
      (begin (repl-interpret-dont-read repl (test-input test)) (test-succeed test "" on-pass))
      (let ((actual (repl-interpret repl (test-input test))))
        (if (ormap (lambda (expected) (string=? expected actual)) (test-expecteds test))
            (test-succeed test actual on-pass)
            (test-fail test actual on-fail)))))

(define (test-succeed test actual thnk)
  (thnk)
  (displayln (format "\tPassed: ~a -> ~a" (test-input test) actual)))

(define (test-fail test actual thnk)
  (thnk)
  (displayln (color-text
              (format "\tFailed: expected ~a -> ~a, but got -> ~a" (test-input test) (car (test-expecteds test)) actual)
              "31")))

(define (color-text text color-code-str)
  (string-append "\033[" color-code-str "m" text "\033[0m"))

(define (test-defns-do repl test on-pass on-fail)
  (repl-interpret-defns repl (test-defns-definitions test))
  (let ((actual (repl-interpret repl (test-input test))))
    (repl-interpret-defns repl (test-defns-resets test))
    (if (ormap (lambda (expected) (string=? expected actual)) (test-expecteds test))
        (test-defns-pass test actual on-pass)
        (test-defns-fail test actual on-fail))))

(define (test-defns-pass test actual on-pass)
  (on-pass)
  (for-each (lambda (defn) (displayln (format "\t\t~a" defn))) (test-defns-definitions test))
  (displayln (format "\tPassed:\t~a -> ~a" (test-input test) actual)))

(define (test-defns-fail test actual on-fail)
  (on-fail)
  (for-each (lambda (defn) (displayln "~a" defn)) (test-defns-definitions test))
  (displayln (format "\tPassed: ~a -> ~a" (test-input test) actual)))

(define (test-seq-do repl test on-pass on-fail)
  (call/cc (lambda (cc)
             (for-each (lambda (seq-body) (test-seq-body-do repl seq-body on-fail cc))
                       (take (test-seq-tests test) (- (length (test-seq-tests test)) 1)))
             (test-do repl (last (test-seq-tests test)) on-pass on-fail))))

(define (test-seq-body-do repl test on-fail return)
  (if (null? (test-expecteds test))
      (begin (repl-interpret-dont-read repl (test-input test))
             (displayln (format "\t\t~a" (test-input test))))
      (let ((actual (repl-interpret repl (test-input test))))
        (if (ormap (lambda (expected) (string=? expected actual)) (test-expecteds test))
            (displayln (format "\t\t~a -> ~a" (test-input test) actual))
            (begin (test-fail test actual on-fail) (return (void)))))))

(struct section (title tests))

;;
(define constants
  (section
   "Constants"
   (list
    (test "#t" '("#t"))
    (test "#f" '("#f"))
    (test "1.1" '("1.1"))
    (test "#\\c" '("#\\c"))
    (test "\"string\"" '("\"string\""))
    (test "0" '("0"))
    (test "-1" '("-1"))
    (test "+1" '("1"))
    (test "1" '("1")))))

(define lambdas
  (section
   "Lambda Expression Applications"
   (list
    (test "((lambda () 1))" '("1"))
    (test "((lambda () 4 3 2 1))" '("1"))
    (test "((lambda (x) x) 123)" '("123"))
    (test "((lambda (x y) y) 1 2)" '("2"))
    (test "((lambda l l) 1 2 3)" '("(1 2 3)"))
    (test "((lambda (x . r) r) 1 2 3)" '("(2 3)"))
    (test "((lambda (x y . r) r) 1 2 3)" '("(3)"))
    (test "((lambda (x) ((lambda () ((lambda () x))))) 1)" '("1"))
    (test "((lambda () (values 1 2 3) 3))" '("3"))
    (test "((lambda () (values) (cons 1 2) 3))" '("3"))
    (test "((lambda () (define a 1) (define b 2) (define c 3) (define d 4) (define e 5) (define f 6) (cons 1 2) (cons 3 '())))" '("(3)"))
    ;; TODO: figure out how to actually pretty print
    (test "(((lambda (x) (x x))
                    (lambda (f)
                      (lambda (xs)
                        (if (null? xs) 0 (+ 1 ((f f) (cdr xs)))))))
                 '(a b c d))" '("4"))
    (test "(((lambda (x) (x x))
                    (lambda (f)
                      (lambda (n)
                        (if (zero? n) 1 (* n ((f f) (+ n -1)))))))
                 5)" '("120")))))

(define calls-w-values
  (section
   "Values and call-with-values"
   (list
    (test "(call-with-values (lambda () (values 1 2 3)) +)"
          '("6"))
    (test "(call-with-values
                  (lambda () (values 1 2 3))
                  (lambda (a . rest) rest))"
          '("(2 3)"))
    (test "(call-with-values
                  (lambda () (values 1 2 3))
                  (lambda (a b . rest) rest))"
          '("(3)"))
    (test "(call-with-values
                  (lambda () (values 1 2))
                  (lambda (a b) (cons a (cons b '()))))" '("(1 2)")))))
(define quotes
  (section
   "Quotation"
   (list
    (test "(quote 1)" '("1"))
    (test "(quote a)" '("a"))
    (test "(quote (1 2 3))" '("(1 2 3)"))
    (test "(quote ())" '("()")))))

(define applies
  (section
   "Apply"
   (list
    (test "(apply + '(1 2 3 4))" '("10"))
    (test "(apply - '(10 5 2))" '("3"))
    (test "(apply (lambda (a b) (cons a (cons b '()))) '(1 2))" '("(1 2)"))
    (test-seq
     (list
      (test "(define k #f)" '())
      (test "(+ 1 (call/cc (lambda (cc) (set! k cc) 2)) 3)" '("6"))
      (test "(apply k '(3))" '("7")))
     (list "(define k (if #f #f))")))))

(define begins
  (section
   "Begin forms"
   (list
    (test "(begin 0 1 2)" '("2"))
    (test "(+ 1 (begin 0 1 2) 3)" '("6"))
    (test "(+ 1 (begin 0 (begin 1 (begin 2))) 3)" '("6"))
    (test "(if (begin #f #t) 1 0)" '("1"))
    (test "((lambda (a) (begin (define b 2) (define c 3)) (+ a b c)) 1)" '("6"))
    (test "((lambda ()
                   (begin (define even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))))
                   (define odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                   (odd? 15)))" '("#t"))
    (test-seq
     (list
      (test "(begin (define a 1) (define b 2) (define c 3))" '())
      (test "b" '("2")))
     (list "(define a)" "(define b)" "(define c)"))
    (test-seq
     (list
      (test "(begin (define a 1) (begin (define b 2) (begin (define c 3))))" '())
      (test "(+ a b c)" '("6")))
     (list "(define a)"  "(define b)" "(define c)"))
    (test "((lambda () (begin (define a 1) (begin (define b 2) (define c 3) (begin (define d 4) (define e 5)) (define f 6) 1) 2) (+ a b c d e f)))" '("21")))))

(define records
  (section
   "Records"
   (list
    ;; procedural api
    (test-seq
     (list
      (test "(define point2d-rtd (make-record-type-descriptor 'point2d #f #f #f #f (vector '(mutable x) '(mutable y))))" '())
      (test "(record-type-descriptor? point2d-rtd)" '("#t"))
      (test "(define point2d-rcd (make-record-constructor-descriptor point2d-rtd #f #f))" '())
      (test "(record-constructor-descriptor? point2d-rcd)" '("#t"))
      (test "(define make-point2d (record-constructor point2d-rcd))" '())
      (test "(define pt (make-point2d 0 1))" '())
      (test "(record? pt)" '("#t"))
      (test "(define point2d-x (record-accessor point2d-rtd 0))" '())
      (test "(define point2d-y (record-accessor point2d-rtd 1))" '())
      (test "(point2d-x pt)" '("0"))
      (test "(point2d-y pt)" '("1"))
      (test "(define point2d? (record-predicate point2d-rtd))" '())
      (test "(point2d? pt)" '("#t"))
      (test "(point2d? (vector 0 1))" '("#f")))
     (list "(define point2d-rtd)" "(define point2d-rcd)" "(define make-point2d)" "(define pt)" "(define point2d-x)"  "(define point2d-y)" "(define point2d?)"))
    ;; Inheritance
    (test-seq
     (list
      (test "(define point2d-rtd (make-record-type-descriptor 'point2d #f #f #f #f (vector '(mutable x) '(mutable y))))" '())
      (test "(define point2d-rcd (make-record-constructor-descriptor point2d-rtd #f #f))" '())
      (test "(define point3d-rtd (make-record-type-descriptor 'point3d point2d-rtd #f #f #f (vector '(mutable z))))" '())
      (test "(define point3d-rcd (make-record-constructor-descriptor point3d-rtd point2d-rcd #f))" '())
      (test "(define make-point3d (record-constructor point3d-rcd))" '())
      (test "(define pt (make-point3d 0 1 2))" '())
      (test "(define point2d? (record-predicate point2d-rtd))" '())
      (test "(point2d? pt)" '("#t"))
      (test "(define point-x (record-accessor point2d-rtd 0))" '())
      (test "(define point-y (record-accessor point2d-rtd 1))" '())
      (test "(define point-z (record-accessor point3d-rtd 0))" '())
      (test "(point-z pt)" '("2"))
      (test "(point-x pt)" '("0"))
      (test "(point-y pt)" '("1")))
     (list "(define point2d-rtd)"
           "(define point2d-rcd)"
           "(define point3d-rtd)"
           "(define point3d-rcd)"
           "(define make-point3d)"
           "(define pt)"
           "(define point2d?)"
           "(define point-z)"
           "(define point-x)"
           "(define point-y)")))))

(define define-syntaxes
  (section
   "Define-syntax forms"
   (list
    (test-seq
     (list
      (test "(define-syntax app
                    (syntax-rules ()
                      ((app op arg ...) (op arg ...))))" '())
      (test "(app + 1 2 3)" '("6")))
     (list)))))

(define hygienic-macros
  (section
   "Hygienic macros"
   (list
    (test-seq
     (list
      (test "(define-syntax or2
                    (syntax-rules ()
                      ((or a b) (let ((x a)) (if x x b)))))" '())
       (test "((lambda (x) (or2 #f x)) #t)" '("#t")))
      (list)))))

(define dynamic-winds
  (section
   "Dynamic-wind"
   (list
    (test "(dynamic-wind
                  (lambda () #f)
                  (lambda () 1)
                  (lambda () #t))"
          '("1"))
    (test "(call-with-values
                  (lambda ()
                    (dynamic-wind
                      (lambda () (values 1 2 3))
                      (lambda () (values 4 5 6))
                      (lambda () (values 7 8 9))))
                  (lambda l l))"
          '("(4 5 6)"))
    ;; NOTE: the below test has an unspecified result, because the order of operand evaluation (in particular for the call to cons) is unspecified
    ;; (test "((lambda (a) (cons (dynamic-wind (lambda () (set! a (+ 1 a))) (lambda () a) (lambda () (set! a (+ 1 a)))) a)) 0)" '("(1 . 2)"))
    (test "((lambda (a)
                   (define b
                           (dynamic-wind
                             (lambda () (set! a (+ 1 a)))
                             (lambda () a)
                             (lambda () (set! a (+ 1 a)))))
                   (cons a b))
                  0)"
          '("(2 . 1)"))
    (test "(+ 1
                   (dynamic-wind
                     (lambda () #f)
                     (lambda () 2)
                     (lambda () #t))
                   3)" '("6"))
    (test-seq
     (list
      (test "(define k #f)" '())
      (test "(define xs '())" '())
      (test "(dynamic-wind
                    (lambda () (set! xs (cons 1 xs)))
                    (lambda () (call/cc (lambda (cc) (set! k cc) 'body)))
                    (lambda () (set! xs (cons 2 xs))))" '("body"))
      (test "(k 'hi)" '("hi"))
      (test "xs" '("(2 1 2 1)")))
     (list "(define k (if #f #f))"))
    (test-seq
     (list
      (test "(define k)" '())
      (test "(define xs '())" '())
      (test "(+ 1 (call/cc (lambda (cc) (set! k cc) 2)) 3)" '("6"))
      (test "(dynamic-wind
                    (lambda () (set! xs (cons 'in xs)))
                    (lambda () (k 38))
                    (lambda () (set! xs (cons 'out xs))))" '("42"))
      (test "xs" '("(out in)")))
     (list "(define k)" "(define xs)"))
    (test-seq
     (list
      (test "(define k)" '())
      (test "(define xs '())" '())
      (test "(dynamic-wind
                    (lambda () (set! xs (cons 'in1 xs)))
                    (lambda () (call/cc (lambda (cc) (set! k cc) 'body1)))
                    (lambda () (set! xs (cons 'out1 xs))))" '("body1"))
      (test "(dynamic-wind
                    (lambda () (set! xs (cons 'in2 xs)))
                    (lambda () (k 'wow))
                    (lambda () (set! xs (cons 'out2 xs))))" '("wow"))
      (test "xs" '("(out1 in1 out2 in2 out1 in1)")))
     (list "(define k)" "(define xs)"))
    (test-seq
     (list
      (test "(define k)" '())
      (test "(define xs '())" '())
      (test "(dynamic-wind
                    (lambda () (set! xs (cons 'in1 xs)))
                    (lambda () (dynamic-wind
                                 (lambda () (set! xs (cons 'in2 xs)))
                                 (lambda () (call/cc (lambda (cc) (set! k cc) 'inner-body)))
                                 (lambda () (set! xs (cons 'out2 xs)))))
                    (lambda () (set! xs (cons 'out1 xs))))" '("inner-body"))
      (test "(k 'wow)" '("wow"))
      (test "xs" '("(out1 out2 in2 in1 out1 out2 in2 in1)")))
     (list "(define k)" "(define xs)"))
    (test-seq
     (list
      (test "(define k)" '())
      (test "(define xs '())" '())
      (test "(dynamic-wind
                    (lambda () (set! xs (cons 'in1 xs)))
                    (lambda () (dynamic-wind
                                 (lambda () (set! xs (cons 'in2 xs)))
                                 (lambda () (call/cc (lambda (cc) (set! k cc) 'inner-body)))
                                 (lambda () (set! xs (cons 'out2 xs)))))
                    (lambda () (set! xs (cons 'out1 xs))))" '("inner-body"))
      (test "(dynamic-wind
                    (lambda () (set! xs (cons 'in3 xs)))
                    (lambda () (dynamic-wind
                                 (lambda () (set! xs (cons 'in4 xs)))
                                 (lambda () (k 'wow))
                                 (lambda () (set! xs (cons 'out4 xs)))))
                    (lambda () (set! xs (cons 'out3 xs))))" '("wow"))
      (test "xs" '("(out1 out2 in2 in1 out3 out4 in4 in3 out1 out2 in2 in1)")))
     (list "(define k)" "(define xs)"))
    )))

(define arithmetic
  (section
   "Arithmetic"
   (list
    (test "(+)" '("0"))
    (test "(+ 1)" '("1"))
    (test "(- 1)" '("-1"))
    (test "(- 10 2 3 4)" '("1"))
    (test "(+ 1 2 3 4)" '("10"))
    (test "(*)" '("1"))
    (test "(* 3)" '("3"))
    (test "(* 1 2 3 4)" '("24"))
    (test "(= 1)" '("#t"))
    (test "(= 1 1)" '("#t"))
    (test "(= 1.1 1.1)" '("#t"))
    (test "(> 3)" '("#t"))
    (test "(> 3 2 1)" '("#t"))
    (test "(> 2 2 1)" '("#f"))
    (test "(> 1 1 2)" '("#f"))
    (test "(< 3)" '("#t"))
    (test "(< 3 2 1)" '("#f"))
    (test "(< 1 1 2)" '("#f"))
    (test "(< 1 2 2)" '("#f"))
    (test "(< 1 2 3)" '("#t"))
    (test "(= 1 1)" '("#t"))
    (test "(= 1.1 1.2)" '("#f"))
    (test "(= 1 1 1 1 1)" '("#t"))
    (test "(= 1 2)" '("#f"))
    (test "(= 1 2 1 1 1 1)" '("#f")))))

(define logics
  (section
   "Logic"
   (list
    (test "(not #f)" '("#t"))
    (test "(not #t)" '("#f"))
    (test "(not 1)" '("#f"))
    (test "(or)" '("#f"))
    (test "(or 1 2)" '("1"))
    (test "(or #f 2)" '("2"))
    (test "(or 1 #f)" '("1"))
    (test "(or #f #f 3)" '("3"))
    (test "(and)" '("#t"))
    (test "(and 1)" '("1"))
    (test "(and 1 2)" '("2"))
    (test "(and 1 #f)" '("#f"))
    (test "(and 1 2 #f)" '("#f")))))

(define lets
  (section
   "Lets"
   (list
    (test "(let ((x 1)) x)" '("1"))
    (test "(let ((x 1) (y 2)) (cons x y))" '("(1 . 2)")))))

(define letstars
  (section
   "Let*s"
   (list
    (test "(let* ((x 12) (y (* 2 x))) (+ x y))" '("36")))))

(define letrecs
  (section
   "Letrecs"
    (list
     (test "(letrec ((len (lambda (xs) (if (null? xs) 0 (+ 1 (len (cdr xs))))))) (len '(a b c d)))" '("4")))))

(define conds
  (section
   "Conds"
   (list
    (test "(cond ((null? '()) 1))" '("1"))
    (test "(cond ((null? '(1)) 1) (#t 2))" '("2"))
    (test "(cond ((null? '(1)) 1) (else 2))" '("2")))))

(define local-definitions
  (section
   "Local definitions"
   (list
    (test "((lambda () (define a 1) a))" '("1"))
    (test "((lambda ()
                    (define even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                    (define odd?  (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                    (odd? 1)))" '("#t"))
    (test-seq
     (list
      (test "(define x 1)" '())
      (test "(define fun
                    (lambda () (define x 2)))" '())
      (test "(fun)" '())
      (test "x" '("1")))
     (list "(set! x (if #f #f))")))))

(define top-level-definitions
  (section
   "Top-level definitions"
   (list
    (test-defns "a" '("1") '("(define a 1)") '("(define a (if #f #f))"))
    (test-defns "(+ a b)" '("3") '("(define a 1)" "(define b 2)") '("(define a (if #f #f))" "(define b (if #f #f))"))
    (test-seq
     (list
      (test "(define a)" '())
      (test "a" '())
      (test "(set! a 1)" '())
      (test "a" '("1")))
     (list "(set! a (if #f #f))")))))

(define local-setbangs
  (section
   "Assignments to local variables"
   (list
    (test "((lambda (a) (set! a 1) a) 0)" '("1"))
    (test "((lambda (p) (set! p (car p)) p) (cons 1 2))" '("1")))))


(define ifs
  (section
   "If Then (Else)"
   (list
    (test "(if #t 1 0)" '("1"))
    (test "(if 1 1 0)" '("1"))
    (test "(if '() 1 0)" '("1"))
    (test "(if '(1 2 3) 1 0)" '("1"))
    (test "(if 1.1 1 0)" '("1"))
    (test "(if \"string\" 1 0)" '("1"))
    (test "(if #f 1 0)" '("0"))
    (test "(if #f #f)" '())
    (test "(if #t 1)" '("1"))
    (test "(if (null? (quote ())) 1)" '("1")))))

(define pairs
  (section
   "Pairs -- cons, car and cdr"
   (list
    (test "(cons 1 2)" '("(1 . 2)"))
    (test "(cons 1 '(2 3 4))" '("(1 2 3 4)"))
    (test "(car (cons 1 2))" '("1"))
    (test "(cdr (cons 1 2))" '("2"))
    (test "(car '(1 2 3 4))" '("1"))
    (test "(cdr '(1 2 3 4))" '("(2 3 4)")))))

(define lists
  (section
   "Lists"
   (list
    (test "(list)" '("()"))
    (test "(list 1 2 3)" '("(1 2 3)"))
    (test "(null? 1)" '("#f"))
    (test "(null? '())" '("#t"))
    (test "(null? '(1))" '("#f"))
    (test "(append)" '("()"))
    (test "(append '() '())" '("()"))
    (test "(append '(1 2) '(3 4) '(5 6 7 8 9))" '("(1 2 3 4 5 6 7 8 9)"))
    (test "(append '(1 2 3) '(4 5 6))" '("(1 2 3 4 5 6)"))
    (test "(append '(1 2 3) '(4 5) 6)" '("(1 2 3 4 5 . 6)"))
    (test "(list? '(a b c))" '("#t"))
    (test "(list? 'a))" '("#f"))
    (test "(list? (cons 'a 'b))" '("#f")))))

(define quasiquotes
  (section
   "Quasiquotation"
   (list
    (test "(quasiquote ())" '("()"))
    (test "(quasiquote x)" '("x"))
    (test "(quasiquote (a b c))" '("(a b c)"))
    (test "(quasiquote (a (+ 1 1) c))" '("(a (+ 1 1) c)"))
    ; TODO: rewrite below as test-seq. Remove test-defns
    (test-defns "(quasiquote (unquote b))" '("2") '("(define b 2)") '("(define b (if #f #f))"))
    (test-defns "(quasiquote (a (unquote b) c))" '("(a 2 c)") '("(define b 2)") '("(define b (if #f #f))"))
    (test-seq
     (list
      (test "(define a 1)" '())
      (test "(define b 2)" '())
      (test "(quasiquote ((unquote-splicing (cons a (cons b '()))) 3 4))" '("(1 2 3 4)")))
     (list "(define a)" "(define b)"))
    (test-seq
     (list
      (test "(define a 1)" '())
      (test "(define b 2)" '())
      (test "(quasiquote (-1 0 (unquote-splicing (cons a (cons b '()))) 3 4))" '("(-1 0 1 2 3 4)")))
     (list "(define a)" "(define b)"))
    (test "(quasiquote (a (unquote (+ 1 1)) c))" '("(a 2 c)")))))

(define calls/cc
  (section
   "Call with current continuation"
   (list
    (test-seq
     (list
      (test "(define k #f)" '())
      (test "(+ 1 (call/cc (lambda (cc) (set! k cc) 2)) 3)" '("6"))
      (test "(k 3)" '("7")))
     (list "(define k (if #f #f))"))
    (test-seq
     (list
      (test "(define k #f)" '())
      (test "(call-with-values
                    (lambda () (call/cc (lambda (cc) (set! k cc) (values 1 2 3))))
                    (lambda (x y z) (+ x (* y z))))" '("7"))
      (test "(k 3 4 5)" '("23")))
     (list "(define k (if #f #f))")))))

; TODO: tests for do
; TODO: tests for when
; TODO: tests for case

(struct repl (out in err proc))

(define (repl-interpret repl input)
  (displayln input (repl-in repl))
  (flush-output (repl-in repl))
  (read-line (repl-out repl)))

(define (repl-interpret-dont-read repl input)
  (displayln input (repl-in repl))
  (flush-output (repl-in repl)))

(define (repl-interpret-defns repl defs)
  ; there's nothing to read from stdout after a definition, so these have to be handled differently from expressions
  (for-each (lambda (def) (repl-interpret-dont-read repl def)) defs))


(define (repl-status repl) ((repl-proc repl) 'status))

(define (repl-wait repl) ((repl-proc repl) 'wait))

(define (repl-close r)
  (close-input-port (repl-out r))
  (close-input-port (repl-err r))
  (close-output-port (repl-in r))
  (repl-wait r))

(define (repl-from-command cmd)
  (let ((proc (process cmd)))
    (repl (car proc) (cadr proc) (cadddr proc) (car (cddddr proc)))))


(define total-failed 0)
(define total-passed 0)

(define (section-procedures)
  (let ((numSuccess 0)
        (numFailure 0))
    (list
     (lambda (section) (displayln (format "*** Section: ~a ***" (section-title section))))
     (lambda (_) (displayln (format "    Passed: ~a. Failed: ~a. Total: ~a ***" numSuccess numFailure (+ numSuccess numFailure))) (newline))
     (lambda (repl the-test)
       (let
           ((on-pass (lambda () (set! numSuccess (+ 1 numSuccess)) (set! total-passed (+ 1 total-passed))))
            (on-fail (lambda () (set! numFailure (+ 1 numFailure)) (set! total-failed (+ 1 total-failed)))))
         (cond
           ((test-defns? the-test) (test-defns-do repl the-test on-pass on-fail))
           ((test-seq? the-test) (test-seq-do repl the-test on-pass on-fail))
           ((test? the-test) (test-do repl the-test on-pass on-fail))
           (else (error))))))))

(define (section-do section repl on-start on-finish run-test)
  (on-start section)
  (for-each (lambda (test) (run-test repl test)) (section-tests section))
  (on-finish section))

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define load-time (current-inexact-milliseconds))
  (define repl (repl-from-command (string-join args " ")))
  (repl-interpret repl "1")
  (set! load-time (- (current-inexact-milliseconds) load-time))
  (define start-time (current-inexact-milliseconds))
  (for-each (lambda (section)
              (let ((procs (section-procedures)))
                (section-do section
                            repl
                            (first procs)
                            (second procs)
                            (third procs))))
            (list constants
                  ifs
                  lambdas
                  top-level-definitions
                  local-definitions
                  local-setbangs
                  begins
                  calls-w-values
                  calls/cc
                  dynamic-winds
                  pairs
                  lists
                  applies
                  quotes
                  quasiquotes
                  arithmetic
                  records
                  define-syntaxes
                  hygienic-macros
                  logics
                  lets
                  letrecs
                  ;; ; letstars
                  conds
                  ))

  (displayln (format "Time to load scheme: ~a" load-time))
  (if (not (= total-failed 0))
      (displayln (color-text
                  (format "Total failed: ~a / ~a. Elapsed ms: ~a" total-failed (+ total-failed total-passed) (- (current-inexact-milliseconds) start-time)) "31"))
      (displayln (color-text
                  (format "Total failed: 0 / ~a. Elapsed ms: ~a" total-passed (- (current-inexact-milliseconds) start-time)) "32")))
  (repl-close repl))

(main)
