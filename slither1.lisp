(defvar *b* NIL) ; Global variable for board
(defvar *r* NIL) ; Global variable for row
(defvar *c* NIL) ; Global variable for column

(defun parseFile (file)
  (let ((in (open file))
  (temp NIL))
    ;(when in 
      (let ((dimensions (read in)))
        (setq *r* (car dimensions))
        (setq *c* (cadr dimensions))
        (loop for line = (read-line in nil)
          while line do 
          (setq temp (append temp (list (substitute #\  #\; (concatenate 'string (concatenate 'string " " line) " ")))))
          (setq temp (append temp (list (addNodes *c*))))))
        ;(setq temp (append temp (list  (addNodes *c*)))))
      (close in);)
    (setq *b* temp)
    *b*))


(defun addNodes (x) 
  (setq s (makeArray ""))
  (loop repeat x do (vector-push-extend #\+ s) (vector-push-extend #\  s)) (vector-push-extend #\+ s) 
  (return-from addNodes s))

(defun makeArray (n)
               (make-array (length n)
                           :fill-pointer (length n)
                           :adjustable t
                           :initial-contents n
                           :element-type (array-element-type n)))

(defun printBoard ()
  (format t "~%")
  (dolist (i *b*) (format t "~a~%" i)))

(defun read-move () 
  (loop 
     (when *game-over-flag*  ; Game-over flag 
       (print "Game Over") (return)) 
     (parse-check-move (write-to-string (read)))))

(defun parse-check-move (move)
  ;; move is of the format RCE (Row-Col-Edge)
  (cond ((= (length move) 5)
   ;; Check if the move is valid. 
   
   (Examine-Execute-Move move)
   (printBoard))

   #|(if (> (incf num-moves) 5) 
       (setq *flag* t))) |#
  (t (print "Wrong move values")) ))

(defun Examine-Execute-Move (move)
  (Calculate-move move)
  (set-edge (nth 1 gridnum))
  (setq *game-over-flag* t))

(defun Calculate-move (mv)
  (let (( b (coerce mv 'list)))
  (setq row (digit-char-p (nth 1 b)))
  (setq col (digit-char-p (nth 2 b)))
  (setq edge (nth 3 b))
    (cond ((and (<= row *r*) (<= col *c*) (if (member edge '(#\U #\L #\R #\D)) t nil))
     (setq gridnum (Get-Cell-Numbers row col edge))
     (print gridnum)
     (return-from Calculate-move gridnum))
    (t (print "Invalid move")))))

(defun Get-Cell-Numbers (row col edge)
  ; should return a list '(cell-num edge-num node-num)
  (setq cell-num (+ (* (- (* 2 row) 1)  (+ (* 2 *c*) 1)) (* 2  col)))
  (cond ((char-equal edge #\U)
   (setq edge-num (- cell-num (+ (* 2 *c*) 1)))
   (setq node-num (list (- edge-num 1) (+ edge-num 1))))

  ((char-equal edge #\L)
   (setq edge-num (- cell-num 1))
   (setq node-num (list (- edge-num (+ (* 2 *c*) 1)) (+ edge-num (+ (* 2 *c*) 1)))))

  ((char-equal edge #\R)
   (setq edge-num (+ cell-num 1))
   (setq node-num (list (- edge-num (+ (* 2 *c*) 1)) (+ edge-num (+ (* 2 *c*) 1)))))

  ((char-equal edge #\D)
   (setq edge-num (+ cell-num (+ (* 2 *c*) 1)))
   (setq node-num (list (- edge-num 1) (+ edge-num 1)))))

  (list cell-num edge-num node-num))

(defun set-edge (edge)
  (setq temp-cols (+ (* *c* 2) 1))
  (let ((r (floor (/ edge temp-cols)))
  (c (mod edge temp-cols)))
    (if (= c 0) (setq c temp-cols))
    (decf c)
    (if (= (mod r 2) 1)
  (setf (char (nth r *b*) c) #\|)
  (setf (char (nth r *b*) c) #\-))))
  

(parseFile "boards.txt")
(printBoard)

(setq *game-over-flag* NIL)     
(read-move)


(defun test ()

  (setq abc (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
  (setf (nth 0 (nth 2 abc)) 10) 
  (print abc))

(test)
