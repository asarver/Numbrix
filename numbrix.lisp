(defun prompt-read (prompt)
    (format *query-io* "~a: " prompt)
    (force-output *query-io*)
    (read-line *query-io*))

(defun numbrix()
  (let ((input (ask-for-file)))
    (play-numbrix input)))

(defun ask-for-file ()
    (return-from ask-for-file (prompt-read
"Hello, welcome to Numbrix.

The goal of the game is to be able to create a linear (no diaganols!) path of numbers starting from 1 to n, where n x n is the size of the board.

The first row is the bottom row, and the first column is on the left.

In order to play the game, enter in your answers by:
        row col number (i.e. 2 2 5)
Where row is from 1 to n, and col is from 1 to n.

You CANNOT change values that were in the original game board.

Good luck!

Please enter in the file you would like to load")))

(defun play-numbrix (input)
    (setf original (list ))
    (setf info (read-file input))
    (if (null info)
      (progn
        (print "There was a problem reading the file. Please try again.")
        (return-from play-numbrix nil)))

    (setf board (car info))
    (setf elements_left (cadr info))
    (setf min_elmt (caddr info))
    (print "min elmt")
    (print min_elmt)
    (terpri)
    (print-board board)

    (loop while (> elements_left 0) do
      (setf input
            (prompt-read "Enter in your position
            \(row col element\)"))
      (setf results (insert-element-into-board input board min_elmt))
      (setf board (car results))
      (setf is_null_value (cadr results))
      (setf min_elmt (caddr results))
      (if is_null_value
        (setf elements_left (- elements_left 1)))
      (print-board board))

      (if (= elements_left 0)
        (progn
          (if (check-if-correct board)
            (princ "You won!")
            (princ "You lost."))
          (setq play_again
            (prompt-read "Would you like to play again? \( '1' or '0'\)"))
          (if (= (parse-integer play_again :junk-allowed t) 1)
            (numbrix)
            (return-from play-numbrix nil)))))

(defun read-file (file_name)
    (if (null (probe-file file_name))
      (return-from read-file nil))
  * (with-open-file (stream file_name)
    (setf dim (parse-integer (read-line stream nil)))
    (setf min_elmt (* dim dim))
    (setf elements_left (* dim dim))
    (setf board (make-array (list dim dim)))
    (do ((line (read-line stream nil)
      (read-line stream nil)))
      ((null line))
      (setf info (insert-element-into-board line board min_elmt))
      (setf board (car info))
      (setf min_elmt (caddr info))

      (setf newlist (split-by-one-space line))
      (setf original (nconc original (list newlist)))

      (setf elements_left (- elements_left 1)))
    (return-from read-file (list board elements_left min_elmt))))

(defun read-file-to-array (file_name)
  (if (null (probe-file file_name))
    (return-from read-file-to-array nil))
  * (with-open-file (stream file_name)
      (setf dim (parse-integer (read-line stream nil)))
      (setf board_array (make-array (list 1 (* dim dim))))
      (setf board (make-array (list dim dim)))
      (setf min_elmt (* dim dim))
      (setf elements_left (* dim dim))
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
        ((null line))
        (set info (insert-element-into-board line board min_elmt))
        (setf board (car info))
        (setf elements_left (- elements left 1)))
      (return-from read-file-to-array (list board board_array elements_left min_elmt))))

(defun depth-first (board starting_element)
  )

(defun solve-game (board)
  (let ((input (ask-for-file)))
    (setf original (list ))
    (setf info (read-file input))
    (if (null info)
      (progn
        (print "There was a problem reading the file. Please try again.")
        (return-from solve-game nil)))

    (setf board (car info))
    (setf elements_left (cadr info))
    (setf min_elmt (caddr info))
    (print "min elmt")
    (print min_elmt)
    (terpri)
    (print-board board)))
