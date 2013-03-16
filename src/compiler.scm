;; symbols designated for compilation
(define compiled-symbols
  '(if cond let let* and or begin case))

;; functions 
;(define constant-functions

;;; expanders

;; 'macro-expand' will expand only one level so this function should expand all macros in given form
;; overrides 'macro-expand-all' from TinyScheme, for which I don't know what exactly does
(define (macro-expand-all form)
  ;; tries to transform given symbol in macro by evaluating it in given environment
  ;; evaluating some symbols can fail (e.g. if, case, cond) so we will catch that error and return
  ;; some value where 'macro?' will fail
  (define (symbol->macro? sym)
	(macro? 
	 (catch #f
	   (eval sym))))

  (cond
    [(atom? form) form]
	[(and (symbol? (car form))
		  (symbol->macro? (car form))
		  (macro-expand-all (macro-expand form)))]
	[else
	  (map macro-expand-all form)]))

(define (closure-expand form)
  (cond
    [(symbol? form)
	 (get-closure-code (eval form))]
	[(closure? form)
	 (get-closure-code form)]
	[else 
	  (cons
	    (get-closure-code (eval (car form)))
	    (cdr form))]))

;; expand all closure code; it calls TinyScheme's specific get-closure-code function, that will
;; return content of scheme defined function
;; NOTE: this doesn't work on recursing functions, or it will loop forever
(define (closure-expand-all form)
  (define (symbol->closure? sym)
	(closure?
	  (catch #f
	    (eval sym))))

  (cond
    [(atom? form) form]
	;; special case when we got '(foo . baz) pair as this is pair but not
	;; list and cdr/car will not work on tail
	[(and (pair? form)
		  (not (list? form))
		  form)]
	[(and (symbol? (car form))
		  (symbol->closure? (car form))
		  (closure-expand-all (closure-expand form)))]
	[else
	  (map closure-expand-all form)]))

;;; simplification

(define (fold-if form)
  (let1 item (cadr form)
	(cond
	  [(or (boolean? item)
		   (list? item)
		   (vector? item)
		   (number? item))
	   (if item
		 (caddr form)
		 ;; case when we have (if #f 'foo)
		 (if (= (length form) 3)
		   'nop
		   (cadddr form)))]
	  [else
	    form])))

;;; compilation driver

(define (compile-form form)
  (println (macro-expand-all form)))

(define (compile-body body)
  (for-each compile-form body))

;;; compiler entry point

(define (compile-file filename)
  (call-with-input-file filename
	(lambda (input-port)
	  (let loop ([x    (read input-port)]
				 [body '()])
		(if (eof-object? x)
		  (compile-body (reverse body))
		  (begin
			(set! body (cons x body))
			(loop
			 (read input-port)
			 body)))))))

;(compile-file "init-2.ss")
;(compile-file "demo.scm")
