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

(defun insert-element-into-board (element board min_elmt)
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
    (if (< elmt min_elmt)
      (return-from insert-element-into-board (list board is_null_value elmt))
    (return-from insert-element-into-board (list board is_null_value min_elmt))))

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
