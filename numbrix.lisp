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
        (setf info (insert-element-into-board line board min_elmt))
        (setf board (car info))
        (setf elements_left (- elements left 1)))
      (return-from read-file-to-array (list board board_array elements_left min_elmt))))

(defun find-next (board element_location direction)
  (let ((elmt (aref board (car element_location) (cadr element_location)))
        (row (car element_location))
        (col (cadr element_location))
        (max_elmt (expt (array-dimension board 0) 2)))
    (setf found nil)
    (setf next elmt)
    (loop while (and (not found)
                     (or (and (<= next max_elmt) (= direction 1))
                         (and (>= next 1) (= direction -1)))) do
      (setf next (+ next direction))
      (setf location (find-elmt board next))
      (if (not (null location))
        (setf found T)))
    (if (or (> next max_elmt) (< next 1))
      (return-from find-next nil))
    (setf ideal_location (- next elmt)))
  (return-from find-next (list (abs ideal_location) location)))
    
(defun depth-first (board element_location direction)
  (if (typep board 'integer)
    (return-from depth-first -1))

  (let ((elmt (aref board (car element_location) (cadr element_location)))
        (row (car element_location))
        (col (cadr element_location)))

  (if (or 
         (and (= direction 1) (= elmt (expt (array-dimension board 0) 2)))
         (and (= direction -1) (= elmt 1)))
    (progn
      (if (check-if-correct board)
        (return-from depth-first board)
    (return-from depth-first (depth-first board (find-elmt board min_elmt) (* -1 direction))))))

    (let ((neighbors (grab-neighbors board element_location))
          (next_info (find-next board element_location direction)))
      (let ((up (car neighbors))
            (left (cadr neighbors))
            (down (caddr neighbors))
            (right (cadddr neighbors))
            (next (+ elmt direction))
            (ideal_location (car next_info))
            (ideal_row (caadr next_info))
            (ideal_col (cadadr next_info)))
        (if (null next_info)
          (progn
          (setf ideal_location (expt (array-dimension board 0) 2))
          (setf ideal_row 0)
          (setf ideal_col 0)))

        (if (not (null up))
          (progn
            (if (= up next)
              (return-from depth-first (depth-first board (list (- row 1) col) direction)))))
        (if (not (null left))
          (progn
            (if (= left next)
              (return-from depth-first (depth-first board (list row (- col 1)) direction)))))
        (if (not (null down))
          (progn
            (if (= down next)
              (return-from depth-first (depth-first board (list (+ row  1) col) direction)))))
        (if (not (null right))
          (progn
            (if (= right next)
              (return-from depth-first (depth-first board (list row (+ col 1)) direction)))))
        (if (or (and (not (null up)) (not (null left)) (not (null down))
                 (not (null right))) (is-elmt-in board next))
          (return-from depth-first -1))
        (if (and (null up)
                 (<= (+ (abs (- ideal_row (- row 1))) (abs (- ideal_col col)))
                     ideal_location))
          (progn
            (setf board (car (insert-into-board
                               (list (- row 1) col next) board)))
             (setf moves (append moves (list (list (- row 1) col next))))
             (setf move_on (depth-first board (list (- row 1) col) direction))
             (if (typep move_on 'integer)
               (progn
                 (insert-into-board (list (- row 1) col nil) board)
                 (setf moves (reverse (cdr (reverse moves)))))
             (return-from depth-first board))))
         (if (and (null left)
                 (<= (+ (abs (- ideal_row row)) (abs (- ideal_col (- col 1))))
                     ideal_location))
           (progn
             (setf board (car (insert-into-board
                                (list row (- col 1) next) board)))
             (setf moves (append moves (list (list row (- col 1) next))))
             (setf move_on (depth-first board (list row (- col 1)) direction))
             (if (typep move_on 'integer)
               (progn
                 (insert-into-board (list row (- col 1) nil) board)
               (setf moves (reverse (cdr (reverse moves)))))
             (return-from depth-first board))))
         (if (and (null down)
                 (<= (+ (abs (- ideal_row (+ row 1))) (abs (- ideal_col col)))
                     ideal_location))
           (progn
             (setf board (car (insert-into-board
                                (list (+ row 1) col next) board)))
             (setf moves (append moves (list (list (+ row 1) col next))))
             (setf move_on (depth-first board (list (+ row 1) col ) direction))
             (if (typep move_on 'integer)
               (progn
                 (insert-into-board (list (+ row 1) col nil) board)
                 (setf moves (reverse (cdr (reverse moves)))))
             (return-from depth-first board))))
         (if (and (null right)
                 (<= (+ (abs (- ideal_row row)) (abs (- ideal_col (+ col 1))))
                     ideal_location))
           (progn
             (setf board (car (insert-into-board
                                (list row (+ col 1) next) board)))
             (setf moves (append moves (list (list row (+ col 1) next))))
             (setf move_on (depth-first board (list row (+ col 1)) direction))
             (if (typep move_on 'integer)
               (progn
                 (insert-into-board (list row (+ col 1) nil) board)
                (setf moves (reverse (cdr (reverse moves)))))
             (return-from depth-first board))))
         (return-from depth-first -1)))))

(defun solve-smarter (board moves)
  (return-from solve-smarter (insert-known-elements board moves)))


(defun solve-game ()
  (let ((input (ask-for-file)))
    (setf original (list ))
    (setf info (read-file input))
    (if (null info)
      (progn
        (print "There was a problem reading the file. Please try again.")
        (return-from solve-game nil)))
    (setf moves nil)
    (setf board (car info))
    (setf elements_left (cadr info))
    (setf min_elmt (caddr info))
    (terpri)
    (setf start (get-universal-time))
    (print-board board)
    (setf result (solve-smarter board moves))
    (setf board (car result))
    (setf moves (cadr result))
    (setf result (solve-smarter board moves))
    (setf board (car result))
    (setf moves (cadr result))
    (setf result (check-ahead board moves))
    (setf board (car result))
    (setf moves (cadr result))
    (setf result (solve-smarter board moves))
    (setf board (car result))
    (setf moves (cadr result))
    (setf result (check-ahead board moves))
    (setf board (car result))
    (setf moves (cadr result))
    (setf result (check-ahead board moves))
    (setf board (car result))
    (setf moves (cadr result))
    (setf result (solve-smarter board moves))
    (setf board (car result))
    (setf moves (cadr result))
    (setf result (check-ahead board moves))
    (setf board (car result))
    (setf moves (cadr result))
    (setf result (solve-smarter board moves))
    (setf board (car result))
    (setf moves (cadr result))
    (setf result (solve-smarter board moves))
    (setf board (car result))
    (setf moves (cadr result))
    (setf result (check-ahead board moves))
    (setf board (car result))
    (setf moves (cadr result))

    (if (and (< min_elmt (/ (expt (array-dimension board 0) 2) 2))
             (> min_elmt 1))
      (progn
      (setf board (depth-first board (find-elmt board min_elmt) -1)))
    (setf board (depth-first board (find-elmt board min_elmt) 1)))
    (print "Game Finished")
    (print-board board)
    (terpri)
    (princ "Time taken (in seconds): ")
    (princ (- (get-universal-time) start))
    (terpri)
    (print "Moves:")
    (loop for move in moves do
          (let ((row (- (array-dimension board 0) (car move)))
                (col (+ 1 (cadr move)))
                (elmt (caddr move)))
            (terpri)
            (princ "row: ")
            (princ row)
            (princ " col: ")
            (princ col)
            (princ " elmt: ")
            (princ elmt)))
    (terpri)
    (let ((play_again
            (prompt-read "Would you like to play again? \( '1' or '0'\)")))
      (if (= (parse-integer play_again :junk-allowed t) 1)
       (solve-game)))))
