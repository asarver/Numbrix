(defun prompt-read (prompt)
    (format *query-io* "~a: " prompt)
    (force-output *query-io*)
    (read-line *query-io*))

(defun numbrix ()
  
  (setq play_again T)
  (loop while play_again do
    (setf original (list ))
    (setf input 
      (prompt-read 
        "Hello, welcome to Numbrix.
        Please enter in the file you would like to load"))
    (setf info (read-file input))
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
      (print-board board)
      (print elements_left))
    
      (if (= elements_left 0)
        (progn
          (if (check-if-correct board)
            (print "You won!")
            (print "You lost."))
          (setq play_again
            (prompt-read "Would you like to play again? \( '1' or '0'\)"))
          (if (= (parse-integer play_again) 1)
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
      (return-from insert-element-into-board (list board nil)))
    (setf newlist (split-by-one-space element))

    (setf row (parse-integer (car newlist)))
    (setf col (parse-integer (cadr newlist)))
    (setf elmt (parse-integer (caddr newlist)))

    (if (not (null (position 
        (list (write-to-string row) (write-to-string col) 
              (write-to-string (aref board (- dim row) (- col 1))))  
              original :test #'equal)))
      (return-from insert-element-into-board (list board nil)))

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
      (terpri)
      (princ #\Space))

        (let ((elmt (row-major-aref board i))) (
          if (null elmt)
            (progn
              (princ #\Space)
              (princ #\Space))
            (progn
              (if (> 10 elmt)
                (progn
                  (princ #\Space)
                  (princ elmt))
                (princ elmt))))))
  (terpri))
