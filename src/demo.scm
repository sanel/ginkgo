(display "XXX")

(if "foo"
  (println "This is foo")
  (begin
	(for i in '(1 2 3 4 5)
	  (println i))))
