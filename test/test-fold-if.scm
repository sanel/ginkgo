(load "unittest.scm")
(load "../src/compiler.scm")

(test: (fold-if '(if #t "3" "x")) "3")
(test: (fold-if '(if #t "3")) "3")
(test: (fold-if '(if #f "3" "x")) "x")

(test:
  (fold-if
    '(if #t
	   (begin
		 (println "works"))
	   (begin
		 (println "fail"))))
    '(begin
	   (println "works")))

(test:
  (fold-if '(if #(1 2 3) "ok" "fail")) "ok")

(test:
  (fold-if '(if #f "fail")) 'nop)

(run-tests)

