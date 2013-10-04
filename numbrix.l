(defun prompt-read (prompt)
(format *query-io* "~a: " prompt)
(force-output *query-io*)
(read-line *query-io*))

(defun numbrix ()
(print "hello, welcome to Numbrix.")
(setf input (prompt-read "Please enter in the file you would like to load. "))
(print "user input")
(print input))

