(defun array-slice (arr row)
  (make-array (array-dimension arr 1)
    :displaced-to arr
    :displaced-index-offset (* row (array-dimension arr 1))))

* (defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
    as j = (position #\Space string :start i)
    collect (subseq string i j)
    while j))
