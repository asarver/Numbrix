(defun prompt-read (prompt)
    (format *query-io* "~a: " prompt)
    (force-output *query-io*)
    (read-line *query-io*))

(defun numbrix ()
    (setf input 
      (prompt-read 
        "Hello, welcome to Numbrix.
        Please enter in the file you would like to load"))
    (setf info (read-file input))
    (setf board (car info))
    (setf elements_left (car (cdr info)))
    (print-board board)

    (loop while (not (<= elements_left 0)) do
      (setf input 
            (prompt-read "Enter in your position 
            \(row col element\)"))
      (setf results (insert-element-into-board input board))
      (setf board (car results))
      (setf null_value (car (cdr results)))
      (if is_null_value
        (setf elements_left (- elements_left 1)))
      (print-board board)
      (print elements_left)
    
      (if (= elements_left 0)
        (check-if-correct board))))

(defun check-if-correct (board) )

(defun insert-element-into-board (element board)
    (setf dim (array-dimension board 0))
    (setf row (parse-integer (subseq element 0 1)))
    (setf col (parse-integer (subseq element 2 3)))
    (setf elmt (parse-integer (subseq element 4)))
    (setf row (- dim row))
    (setf col (- col 1))
    (setf is_null_value (null (aref board row col)))
    (setf (aref board row col) elmt)
    (return-from insert-element-into-board (list board is_null_value)))

(defun read-file (file_name)
  * (with-open-file (stream file_name)
    (setf dim (parse-integer (read-line stream nil)))
    (setf elements_left (* dim dim))
    (setf board (make-array (list dim dim)))
    (do ((line (read-line stream nil)
      (read-line stream nil)))
      ((null line))
      (setf board (car (insert-element-into-board line board)))
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
