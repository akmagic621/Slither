; 1. All cells have proper number of edges
; 2. Loop check

;(setq a (list "+ + +" " 3 3 " "+ + +" "     " "+ + +"))

;(dolist (i a) (format t "~A~%" i))

; Loading a file with the board data
(setq file "boards.txt")
(setq dimensions ())
(setq loopcount 0)

; Global variable for board
(defvar *b* NIL)

; Parses file with board data and constructs lists 
(defun parseFile (file)
	(let ((in (open file))
		(temp NIL))
		(when in
			(let ((dimensions (read in))
					(row (car dimensions))
					(col (cadr dimensions)))
			(loop for line = (read-line in nil)
				while line do
					(setf temp (append temp (list line)))
					;(setf temp (append temp (list "+ + + + + +")))))
					(loop while (>= col 0)
						(setf temp (append temp (list "+")))
						(setq col (- 1 col)))))
					;(loop for loopcount from 0 to row
					;	(setf temp (append temp (list "+"))))))
	(close in))
	(setq *b* temp)))

; Print board
(defun printBoard ()
	(dolist (i *b*) (format t "~a~%" i)))

(parseFile file)
(printBoard)

;(defun slither-config-parse (file)
;  (let ((in (open file))
;	(local-board NIL))
;   (when in 
;      (let ((rows-cols (read in))
;	    (rows (car rows-cols))
;	    (cols (cadr rows-cols)))
;	    (loop for line = (read-line in nil)
;	       while line do 
;		 (setf local-board (append local-board (list "+ + + + + +")))
;		 (setf local-board (append local-board (list line)))))
;	    (close in))
 ;  (setq *board* local-board)))	
