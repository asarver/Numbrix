(defun prompt-read (prompt)
    (format *query-io* "~a: " prompt)
    (force-output *query-io*)
    (read-line *query-io*))

(defun numbrix ()

  (setq play_again T)
  (loop while play_again do
    (setf original (list ))
    (setf input (prompt-read
"Hello, welcome to Numbrix.

The goal of the game is to be able to create a linear (no diaganols!) path of numbers starting from 1 to n, where n x n is the size of the board.

The first row is the bottom row, and the first column is on the left.

In order to play the game, enter in your answers by:
        row col number (i.e. 2 2 5)
Where row is from 1 to n, and col is from 1 to n.

You CANNOT change values that were in the original game board.

Good luck!

Please enter in the file you would like to load"))

    (setf info (read-file input))
    (if (null info)
      (progn
        (print "There was a problem reading the file. Please try again.")
        (return-from numbrix nil)))

    (setf board (car info))
    (setf elements_left (car (cdr info)))
    (print-board board)

    (loop while (> elements_left 0) do
      (setf input
            (prompt-read "Enter in your position
            \(row col element\)"))
      (setf results (insert-element-into-board input board))
      (setf board (car results))
      (setf is_null_value (car (cdr results)))
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
            (setq play_again T)
            (setq play_again nil))))))

(defun check-if-correct (board)
  (let ((index (find-one board)))
    (check-board board index)))

(defun check-board (board start_index)
  (let ((neighbors (grab-neighbors board start_index))
        (elmt (aref board (car start_index) (car (cdr start_index))))
        (row (car start_index))
        (col (car (cdr start_index)))
        (max (expt (array-dimension board 0) 2) ))
    (loop for i in neighbors do
        (if (and (not (null i)) (= (+ elmt 1) i))
	  (progn
            (let ((loc (position i neighbors)))
              (if (= loc 0)
                (return-from check-board
                  (check-board board (list (- row 1) col))))
              (if (= loc 1)
                (return-from check-board
                  (check-board board (list row (- col 1)))))
              (if (= loc 2)
                (return-from check-board
                  (check-board board (list (+ row 1) col 1))))
              (if (= loc 3)
                (return-from check-board
                  (check-board board (list row (+ col 1)))))))))
        (if (= elmt max)
          (return-from check-board T))
    (return-from check-board nil)))

(defun grab-neighbors (board index)
  (let ((row (car index)) (col (car (cdr index))))
    (if (= row 0)
      (setq up nil)
      (setq up (aref board (- row 1) col)))
    (if (= col 0)
      (setq left nil)
      (setq left (aref board row (- col 1))))
    (if (= row (- (array-dimension board 1) 1))
      (setq down nil)
      (setq down (aref board (+ row 1) col)))
    (if (= col (- (array-dimension board 1) 1))
      (setq right nil)
      (setq right (aref board row (+ col 1))))
    (list up left down right)))

(defun find-one (board)
  (loop for i below (array-total-size board) do
    (if (not (null (position 1 (array-slice board i))))
      (return-from find-one (list i (position 1 (array-slice board i)))))))

(defun array-slice (arr row)
  (make-array (array-dimension arr 1)
    :displaced-to arr
    :displaced-index-offset (* row (array-dimension arr 1))))

(defun insert-element-into-board (element board)
    (setf dim (array-dimension board 0))
    (if (> 5 (length element))
      (progn
        (princ "Please enter in a valid row col elmt tuple.")
        (return-from insert-element-into-board (list board nil))))
    (setf newlist (split-by-one-space element))

    (setf row (parse-integer (car newlist) :junk-allowed t))
    (setf col (parse-integer (cadr newlist) :junk-allowed t))
    (setf elmt (parse-integer (caddr newlist) :junk-allowed t))

    (if (or (null row) (null col) (null elmt))
      (progn
        (princ "Please enter in a valid row col elmt tuple.")
        (return-from insert-element-into-board (list board nil))))

    (if (or (> row dim) (> col dim) (<= row 0) (<= col 0))
      (progn
        (format *query-io* "Sorry ~a ~a is not a valid position" row col)
        (return-from insert-element-into-board (list board nil))))

    (if (not (null (position
        (list (write-to-string row) (write-to-string col)
              (write-to-string (aref board (- dim row) (- col 1))))
              original :test #'equal)))
      (progn
        (princ "You cannot change an original value.")
        (return-from insert-element-into-board (list board nil))))

    (setf row (- dim row))
    (setf col (- col 1))
    (setf is_null_value (null (aref board row col)))
    (setf (aref board row col) elmt)
    (return-from insert-element-into-board (list board is_null_value)))

* (defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
    as j = (position #\Space string :start i)
    collect (subseq string i j)
    while j))

(defun read-file (file_name)
    (if (null (probe-file file_name))
      (return-from read-file nil))
  * (with-open-file (stream file_name)
    (setf dim (parse-integer (read-line stream nil)))
    (setf elements_left (* dim dim))
    (setf board (make-array (list dim dim)))
    (do ((line (read-line stream nil)
      (read-line stream nil)))
      ((null line))
      (setf board (car (insert-element-into-board line board)))

      (setf newlist (split-by-one-space line))
      (setf original (nconc original (list newlist)))

      (setf elements_left (- elements_left 1)))
    (return-from read-file (list board elements_left))))

(defun print-board (board)
  (loop for i below (array-total-size board) do
    (if (zerop (mod i (array-dimension board 0)))
      (progn
        (terpri)
            (if (< 10 (- (array-dimension board 0) (/ i (array-dimension board 0))))
              (princ "[")
            (princ "[ "))
        (princ (concatenate 'string
                            (write-to-string (- (array-dimension board 0)
                                                (/ i (array-dimension board 0)))) "] " )))
      (progn (princ #\Space)))

        (let ((elmt (row-major-aref board i))) (
          if (null elmt)
            (progn
              (princ #\Space)
              (princ #\Space)
              (princ #\Space)
              (princ #\Space))
            (progn
              (if (< 10 elmt)
                (progn
                  (if (< 100 elmt)
                    (progn
                    (princ elmt)
                    (princ #\Space))
                  (progn
                    (princ #\Space)
                    (princ elmt)
                    (princ #\Space))))
              (progn
                (princ #\Space)
                (princ #\Space)
                (princ elmt)
                (princ #\Space)))))))
  (terpri)
  (princ #\Space)
  (princ #\Space)
  (princ #\Space)
  (princ #\Space)
  (loop for i below (array-dimension board 0) do
    (if (< 1 (/ i 10))
      (princ "[ ")
    (princ "[  "))
    (princ (concatenate 'string
            (write-to-string (+ i 1)) "]" )))
  (terpri)
    (princ #\Space))
