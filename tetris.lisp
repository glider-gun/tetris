;;;; tetris.lisp
(in-package #:tetris)

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

(defstruct mino
  (shape #2A() :type array)
  (color :normal :type color))

(defun rotate-shape-right (shape)
  (destructuring-bind (w h) (array-dimensions shape)
    (let ((new (make-array (list h w))))
      (loop for x below w do
	   (loop for y below h do
		(setf (aref new (- h y 1) x) (aref shape x y) )))
      new)))

(defun rotate-shape-left (shape)
  (destructuring-bind (w h) (array-dimensions shape)
    (let ((new (make-array (list h w))))
      (loop for x below w do
	   (loop for y below h do
		(setf (aref new y (- w x 1)) (aref shape x y) )))
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

(defparameter +TOP-ROOM+ 4)
(defparameter +BOARD-WIDTH+ 8)
(defparameter +BOARD-HEIGHT+ 10)
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
	(incf (game-score *game*) (* 1000 (expt (length indices) 2)))
	(show-game)))))

(defun new-mino-y0 (shape)
  (destructuring-bind (w h) (array-dimensions shape)
    (- +TOP-ROOM+
       (loop for y from (1- h) downto 0
	  until (loop for x below w thereis (aref shape x y))
	  finally (return y)))))

(defun check-finish (game)
  (or (game-overp game)
      (let* ((board (game-board game))
	     (w (array-dimension board 0)))
	(loop for x below w thereis (aref board x (1- +TOP-ROOM+))))))

;; (defmacro with-game-lock (&body body)
;;   `(unwind-protect
;; 	(progn
;; 	  (let ((start (get-universal-time)))
;; 	    (loop for prev = (sb-ext:compare-and-swap (car (game-lock *game*)) nil t)
;; 	       while prev do (sleep 1e-3)
;; 	       when (> (- (get-universal-time) start) 3) do (cerror "from with-game-lock" "timeout")))
;; 	  ,@body)
;;      (setf (car (game-lock *game*)) nil)))

(defmacro with-game-lock (&body body)
  `(sb-thread:with-mutex ((game-lock *game*))
    ,@body))

(defun progress-game ()
  (with-slots (board next-mino mino-and-position) *game*
    (if mino-and-position
	(if (check-going-down)
	    (go-down)
	    (progn (put-mino)
		   (setf mino-and-position nil)
		   (eliminate-lines (eliminatable-lines))))
	(setf mino-and-position
	      (list next-mino (list 3 (new-mino-y0 (mino-shape next-mino))))
	      next-mino
	      (random-mino)))))

(defvar *game*)
(defvar *progress-thread*)

(defun progress-game-func (window)
  (unless (check-finish *game*)
    (with-game-lock
      (let ((cl-charms:*STANDARD-WINDOW* window))
	(progress-game)
	(show-game)))
    (sleep *timeout*)
    (progress-game-func window)))

(alexandria:define-constant +additional-colors+
  '((:orange 1000 550 0)
    (:purple 1000 0 1000))
  :test #'equal)

(alexandria:define-constant +colors+
    '(:black :red :green :yellow :blue :magenta :cyan :white :orange :purple)
  :test #'equal
  :documentation "default colors for showing game")

(alexandria:define-constant +alternative-colors+
  '(:black :red :green :yellow :blue :magenta :cyan :white)
  :test #'equal
  :documentation "colors for showing game, when couldn't change terminal colors")

(alexandria:define-constant +mino-colors+ '(:cyan :blue :orange :yellow :green :purple :red) :test #'equal)
(alexandria:define-constant +alternative-mino-colors+ '(:cyan :blue :magenta :yellow :green :white :red) :test #'equal)

(defvar *mino-colors* +mino-colors+)
(defvar *colors* +colors+)

(defun main ()
  (declare (optimize debug))
  (setf *random-state* (make-random-state t))
  ;; (unless (console:can-change-color)
  ;;   (setf *colors* +alternative-colors+
  ;; 	  *mino-colors* +alternative-mino-colors+))
  (console:with-color-console
      (if (console:can-change-color)
	  +additional-colors+
	  nil)
      (loop for c in (if (console:can-change-color) +colors+ +alternative-colors+)
	 collect (list c c :default))

    (setf *game* (make-game :board
			    (make-array (list +BOARD-WIDTH+ (+ +BOARD-HEIGHT+ +TOP-ROOM+))
					:initial-element nil)
			    :next-mino (random-mino)
			    :mino-and-position nil
			    :lock (sb-thread:make-mutex)))
    (setf *progress-thread* (sb-thread:make-thread
			     #'progress-game-func
			     :arguments (list cl-charms:*STANDARD-WINDOW*)))

    (unless (console:can-change-color)
      (setf *mino-colors* +alternative-mino-colors+))
    ;; (sb-ext:schedule-timer *timer* *timeout*)
    (loop
       do (let ((cmdchar (console:get-command)))
	    (with-game-lock
	      (case cmdchar
    ((#\d :right) (and (check-going-right)
			     (go-right)))
		((#\a :left) (and (check-going-left)
			    (go-left)))
		((#\s :down) (and (check-going-down)
			    (go-down)))
		((#\w :up)   (and (check-rotating-right)
			    (rotate-right)))
		(#\space (and (check-rotating-left)
			      (rotate-left)))
		((#\q #\esc)
		 (setf (game-overp *game*) t)
		 (return)))
	      (when cmdchar
		(show-game))))
       until (check-finish *game*)
       finally (progn (sb-thread:join-thread *progress-thread*) (show-game) (sleep 3))))
  (format t "~D~%" (game-score *game*))
  (finish-output))

(defun show-game ()
  (destructuring-bind (w h) (array-dimensions (game-board *game*))
    (labels ((show-board ()
	       (loop for y from (- +TOP-ROOM+ +TOP+) below h do
		    (progn
		      (console:move-to +LEFT+ (+ +TOP+ (- y +TOP-ROOM+)))
		      (console:write-at-cursor (if (>= y +TOP-ROOM+) "|" " ") :color-pair :white))
		    (loop for x below w
		       for c = (aref (game-board *game*) x y)
		       do (if c
			      (console:write-at-cursor #\* :color-pair c)
			      (console:write-at-cursor " ")))
		    (when (>= y +TOP-ROOM+)
		      (console:write-at-cursor "|" :color-pair :white)))
	       (console:move-to +LEFT+ (+ +TOP+ (- +TOP-ROOM+) h))
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
			     (progn (console:move-to (+ +LEFT+ 1 x0 dx) (+ +TOP+ y0 (- +TOP-ROOM+) dy))
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

(defun make-tetris-command (&optional (name "tetris"))
  (sb-ext:save-lisp-and-die
   name
   :compression t
   :toplevel #'main
   :executable t))
