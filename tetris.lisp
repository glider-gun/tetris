;;;; tetris.lisp
(declaim (optimize (debug 3) (speed 0)))

;;

(in-package #:tetris)

;;; "tetris" goes here. Hacks and glory await!


(deftype color ()
  '(member :black :red :green :yellow :blue :magenta :cyan :white :orange :purple :normal))

(defstruct game
  (score 0 :type fixnum)
  (board #2A() :type array)
  (next-mino ())
  (mino-and-position ())
  (lock nil)
  (overp nil))

(defun bit->bool (d)
  (ecase d
    (0 nil)
    (1 t)))
(defun mino-list->array (mino-list)
  (make-array (list (length (car mino-list))
		    (length mino-list))
	      :initial-contents
	      (mapcar (lambda (l) (mapcar #'bit->bool l))
		      mino-list)))

(defparameter *mino-shapes*
  (mapcar #'mino-list->array
  '(((0 0 0 0)
     (1 1 1 1)
     (0 0 0 0)
     (0 0 0 0))
    ((0 0 0)
     (1 0 0)
     (1 1 1))
    ((0 0 0)
     (0 0 1)
     (1 1 1))
    ((0 0 0)
     (0 1 1)
     (1 1 0))
    ((0 0 0)
     (1 1 0)
     (0 1 1))
    ((0 0 0)
     (0 1 0)
     (1 1 1))
    ((1 1)
     (1 1)))))

;; (defvar *mino-colors* '(:cyan :blue :orange :yellow :green :purple :red))
(defvar *mino-colors* '(:cyan :blue :magenta :yellow :green :white :red))

(defstruct mino
  (shape #2A() :type array)
  (color :normal :type color))

(defun rotate-shape-right (shape)
  (destructuring-bind (w h) (array-dimensions shape)
    (let ((new (make-array (list h w))))
      (loop for x below w do
	   (loop for y below h do
		(setf (aref new y (- w x 1)) (aref shape x y) )))
      new)))

(defun rotate-shape-left (shape)
  (destructuring-bind (w h) (array-dimensions shape)
    (let ((new (make-array (list h w))))
      (loop for x below w do
	   (loop for y below h do
		(setf (aref new (- h y 1) x) (aref shape x y) )))
      new)))

(defun random-mino ()
  (let* ((i (random (length *mino-shapes*))))
    (make-mino :shape (nth i *mino-shapes*)
	       :color (nth i *mino-colors*))))

(defmacro with-board-params ((x0 y0 mw mh bw bh position mino board) game &body body)
    `(let ((,position (second (game-mino-and-position ,game)))
	   (,mino (first (game-mino-and-position ,game)))
	   (,board (game-board ,game)))
       (when ,position
	 (destructuring-bind (,x0 ,y0) ,position
	   (destructuring-bind (,mw ,mh) (array-dimensions (mino-shape ,mino))
	     (destructuring-bind (,bw ,bh) (array-dimensions ,board)
	       ,@body))))))

(defconstant +LEFT+ 5)
(defconstant +TOP+ 3)
(defconstant +BETWEEN+ 5)


(defparameter *timeout* 0.5)



(defun put-mino ()
  (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
    (loop for dx below mw do
	 (loop for dy below mh do
	      (if (aref (mino-shape mino) dx dy)
		  (setf (aref board (+ x0 dx) (+ y0 dy))
			(mino-color mino)))))))

(defun mino-puttable-p (board shape x0 y0)
  (let ((bw (array-dimension board 0))
	(bh (array-dimension board 1))
	(mw (array-dimension shape 0))
	(mh (array-dimension shape 1)))
    (loop for dx below mw always
	 (loop for dy below mh never
	      (and (aref shape dx dy)
		   (or (not (< -1 (+ x0 dx) bw))
		       (not (< -1 (+ y0 dy) bh))
		       (aref board (+ x0 dx) (+ y0 dy))))))))
    

(defun check-going-down ()
  (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
    (mino-puttable-p board (mino-shape mino) x0 (1+ y0))))

(defun check-going-right ()
  (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
    (mino-puttable-p board (mino-shape mino) (1+ x0) y0)))

(defun check-going-left ()
  (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
    (mino-puttable-p board (mino-shape mino) (1- x0) y0)))

(defun check-rotating-right ()
  (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
    (mino-puttable-p board (rotate-shape-right (mino-shape mino)) x0 y0)))

(defun check-rotating-left ()
  (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
    (mino-puttable-p board (rotate-shape-left (mino-shape mino)) x0 y0)))

(defmacro game-position ()
  `(cadr (game-mino-and-position *game*)))

(defmacro game-mino-shape ()
  `(mino-shape (car (game-mino-and-position *game*))))

(defun go-down ()
  (incf (second (game-position))))
(defun go-right ()
  (incf (first (game-position))))
(defun go-left ()
  (decf (first (game-position))))
(defun rotate-right ()
  (setf (game-mino-shape) (rotate-shape-right (game-mino-shape))))
(defun rotate-left ()
  (setf (game-mino-shape) (rotate-shape-left (game-mino-shape))))

(defun eliminatable-lines ()
  ;return indices of already filled rows
  (let ((board (game-board *game*)))
    (loop for y below (array-dimension board 1)
       when (loop for x below (array-dimension board 0) always (aref board x y))
       collect y)))

(defun eliminate-lines (indices)
  (when indices
    (let ((board (game-board *game*)))
      (destructuring-bind (w h) (array-dimensions board)
	(declare (ignore h))
	(loop repeat 3
	   do (progn (loop for y in indices
			do (loop for x below w
			      do (setf (aref board x y)
				       (nth (random 3) '(:red :green :yellow)))))
		     (show-game)
		     (sleep 0.1)))
	(loop for y in indices
	   do (loop for x below w
		 do (setf (aref board x y)
			  nil)))
	(loop for y0 in (sort (copy-list indices) #'>)
	   for dy from 0
	   do (loop for y from (+ y0 dy) downto 1
		 do (loop for x below w
		       do (setf (aref board x y) (aref board x (1- y)))))
	   do (loop for x below w
		 do (setf (aref board x 0) nil)))
	(incf (game-score *game*) 1000)
	(show-game)))))

(defun progress-game ()
  (with-slots (board next-mino mino-and-position) *game*
    (if mino-and-position
	(destructuring-bind (mino position) mino-and-position
	  (if (check-going-down)
	      (go-down)
	      (progn (put-mino)
		     (eliminate-lines (eliminatable-lines))
		     (setf mino-and-position nil))))
	(setf mino-and-position
	      (list next-mino (list 3 0))
	      next-mino
	      (random-mino)))))

(defun check-finish (game)
  (or (game-overp game)
      (let* ((board (game-board game))
	     (w (array-dimension board 0)))
	(loop for x below w thereis (aref board x 0)))))

(defmacro with-game-lock (&body body)
  `(sb-thread:with-mutex ((game-lock *game*))
     ,@body))
     
(defvar *game*)
(defvar *timer*)

(defun get-command-char ()
  (handler-case
      (sb-ext:with-timeout *timeout*
	(peek-char))
    (sb-ext:timeout () nil)))

(defun progress-game-timer-func ()
  (unless (check-finish *game*)
    (progress-game)
    (show-game)
    (sb-ext:schedule-timer *timer* *timeout*)))


(defun main ()
  (setf *game* (make-game :board
			  (make-array '(8 10) :initial-element nil)
			  :next-mino (random-mino)
			  :mino-and-position nil
			  :lock (sb-thread:make-mutex))
	*timer* (sb-ext:make-timer #'progress-game-timer-func))
  (console:with-color-console
      nil
      (loop for c in '(:black :red :green :yellow :blue :magenta :cyan :white)
	 collect (list c c :black))
    (sb-ext:schedule-timer *timer* *timeout*)
    (loop
       do (let ((cmdchar (console:get-command)))
	 (with-game-lock
	   (case cmdchar
	     (:right (and (check-going-right)
			  (go-right)))
	     (:left (and (check-going-left)
			 (go-left)))
	     (:down (and (check-going-down)
			 (go-down)))
	     (:up   (and (check-rotating-right)
			 (rotate-right)))
	     (#\space (and (check-rotating-left)
			  (rotate-left)))
	     ((#\q #\etx)
	      (setf (game-overp *game*) t)
	      (return)))
	   (when cmdchar
	     (show-game))))
       until (check-finish *game*)
       finally (progn (show-game) (sleep 3))))
  (format t "~D~%" (game-score *game*))
  (finish-output))

(defun foo ()
  (console:with-color-console
      nil
      '((:blue :blue :black)
	(:red :red :black))
      (loop for i below 5
	 for cmd = (loop for a = (console:get-command)
			until a finally (return a))
	 do (console:write-at-cursor cmd :color-pair :red)
	 do (console:write-at-cursor " "))))

;; (foo)
  

(defun show-game ()
  (destructuring-bind (w h) (array-dimensions (game-board *game*))
    (labels ((show-board ()
	       (loop for y below h do
		    (progn
		      (console:move-to +LEFT+ (+ +TOP+ y))
		      (console:write-at-cursor "|" :color-pair :white))
		    (loop for x below w
		       for c = (aref (game-board *game*) x y)
		       do (if c
			      (console:write-at-cursor #\* :color-pair c)
			      (console:write-at-cursor " ")))
		    (console:write-at-cursor "|" :color-pair :white))
	       (console:move-to +LEFT+ (+ +TOP+ h))
	       (console:write-at-cursor "+" :color-pair :white)
	       (loop for x below w
		  do (console:write-at-cursor "-" :color-pair :white))
	       (console:write-at-cursor "+" :color-pair :white))
	     (show-score ()
	       (console:move-to (+ +LEFT+ (car (array-dimensions (game-board *game*))) 2 +BETWEEN+) +TOP+)
	       (console:write-at-cursor (format nil "score: ~d" (game-score *game*))))
	     (show-next-mino ()
	       (let ((mino (game-next-mino *game*))
		     (board-w (car (array-dimensions (game-board *game*)))))
		 (console:move-to (+ +LEFT+ board-w 2 +BETWEEN+) (+ +TOP+ 2))
		 (console:write-at-cursor "next:")
		 (destructuring-bind (w h) (array-dimensions (mino-shape mino))
		   (loop for y below h
		      do (console:move-to (+ +LEFT+ board-w 2 +BETWEEN+ 6) (+ +TOP+ 2 y))
		      do (loop for x below w do
			      (if (aref (mino-shape mino) x y)
				  (console:write-at-cursor "*" :color-pair (mino-color mino))
				  (console:write-at-cursor " ")))))))
	     (show-mino-in-progress ()
	       (when (game-mino-and-position *game*)
		 (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
		   (loop for dx below mw do
			(loop for dy below mh do
			     (progn (console:move-to (+ +LEFT+ 1 x0 dx) (+ +TOP+ y0 dy))
				    (when (aref (mino-shape mino) dx dy)
				      (console:write-at-cursor "*" :color-pair (mino-color mino)))))))))
	     (show-game-over ()
	       (console:move-to +LEFT+ (+ +TOP+ (array-dimension (game-board *game*) 1) 2))
	       (console:write-at-cursor "game over")))
    (console:clear)
    (show-next-mino)
    (show-board)
    (show-mino-in-progress)
    (show-score)
    (when (check-finish *game*)
      (show-game-over))
    (console:refresh))))
