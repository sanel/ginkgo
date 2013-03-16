;;; simple testing framework

(define *registered-tests* '())

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (test-print . args)
  (for-each display args))

(define-macro (test: expected received)
  `(test-equal (format "~A == ~A" ',expected ',received)
	,expected
	,received))

(define (test-equal name expected received)
  (cond
    [(equal? expected received)
	 (set! *tests-passed* (+ 1 *tests-passed*))
	 (set! *registered-tests* (cons (list name #t) *registered-tests*))]
	[else
	 (set! *tests-failed* (+ 1 *tests-passed*))
	 (set! *registered-tests* (cons (list name #f) *registered-tests*))]))

(define (run-tests)
  (set! *registered-tests* (reverse *registered-tests*))
  (define i 1)

  (for-each
    (lambda (item)
	  (if (cadr item)
		(test-print "OK       ")
		(test-print "FAILED   "))
	  (test-print "test #" i ": " (car item) "\n")
	  (set! i (+ 1 i)))
	*registered-tests*))
