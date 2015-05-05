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

(defvar *mino-colors* '(:cyan :blue :orange :yellow :green :purple :red))

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

(defun show-game ()
  (destructuring-bind (w h) (array-dimensions (game-board *game*))
    (labels ((show-board ()
	       (loop for y below h do
		    (progn
		      (set-pos +LEFT+ (+ +TOP+ y))
		      (princ (style "|" :color :white))
		      (loop for x below w
			 for c = (aref (game-board *game*) x y)
			 do (princ (if c
				       (style "*" :color c)
				       " ")))
		      (format t (style "|" :color :white))))
	       (set-pos +LEFT+ (+ +TOP+ h))
	       (princ (style "+" :color :white))
	       (loop for x below w
		  do (princ (style "-" :color :white)))
	       (princ (style "+" :color :white)))
	     (show-score ()
	       (set-pos (+ +LEFT+ (car (array-dimensions (game-board *game*))) 2 +BETWEEN+) +TOP+)
	       (format t "score: ~d" (game-score *game*)))
	     (show-next-mino ()
	       (let ((mino (game-next-mino *game*))
		     (board-w (car (array-dimensions (game-board *game*)))))
		 (set-pos (+ +LEFT+ board-w 2 +BETWEEN+) (+ +TOP+ 2))
		 (format t "next:")
		 (destructuring-bind (w h) (array-dimensions (mino-shape mino))
		   (loop for y below h
		      do (set-pos (+ +LEFT+ board-w 2 +BETWEEN+ 6) (+ +TOP+ 2 y))
		      do (loop for x below w do
			      (princ (if (aref (mino-shape mino) x y)
					 (style "*" :color (mino-color mino))
					 " ")))))))
	     (show-mino-in-progress ()
	       (when (game-mino-and-position *game*)
		 (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
		   (loop for dx below mw do
			(loop for dy below mh do
			     (progn (set-pos (+ +LEFT+ 1 x0 dx) (+ +TOP+ y0 dy))
				    (when (aref (mino-shape mino) dx dy)
				      (princ (style "*" :color (mino-color mino)))))))))))
    (clear)
    (show-next-mino)
    (show-board)
    (show-mino-in-progress)
    (show-score)
    (finish-output)
    )))

;; console
(defparameter +ESC+ (format nil "~c[" #\esc))
(defparameter +STDIN_FD+ (sb-sys:fd-stream-fd sb-sys:*stdin*))

(defun color-spec (color &optional bg)
  (declare (color color))
  (format nil (if bg "4~a" "3~a")
    (ecase color
      (:black   0)
      (:red     1)
      (:green   2)
      (:yellow  3)
      (:blue    4)
      (:magenta 5)
      (:cyan    6)
      (:white   7)
      (:orange  "8;5;202")
      (:purple  "8;5;129")
      (:normal  9))))

(defun style (x &key (color :normal)
		  (bgcolor :normal)
		  bold inverse underline)
  (declare (color color bgcolor))
  (format nil "~a~{~d;~}~a;~am~a~a0m"
	  +ESC+
	  (remove nil (list (and bold 1)
			    (and underline 4)
			    (and inverse 7)))
	  (color-spec color)
	  (color-spec bgcolor t)
	  x
	  +ESC+))

(defun clear (&key line)
  (if line
      (format t "~a2K" +ESC+)
      (format t "~a2J" +ESC+)))

(defun set-pos (x y)
  (format t "~a~d;~dH" +ESC+ y x))

(sb-alien:define-alien-routine ("cfmakeraw" %cfmakeraw)
    sb-alien:void (termios* (* t)))

(defun cfmakeraw ()
  (let ((termios (sb-posix::allocate-alien-termios)))

    (%cfmakeraw termios)
    (unwind-protect
	 (sb-posix::alien-to-termios termios)
      (sb-posix::free-alien-termios termios))))

(defmacro with-raw-mode (&body body)
  (let ((old (gensym)))
    `(locally
	 (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (let ((,old (sb-posix:tcgetattr +STDIN_FD+)))
	 (unwind-protect
	      (locally
		  (declare (sb-ext:unmuffle-conditions sb-ext:compiler-note))
		(sb-posix:tcsetattr +STDIN_FD+ sb-posix:tcsadrain
				    (cfmakeraw))
		,@body)
	   (sb-posix::tcsetattr +STDIN_FD+ sb-posix:tcsanow ,old))))))

(defparameter *timeout* 0.2)

(defmacro command-loop ((cmd) &body body)
  (let ((recur (gensym)))
    `(block nil
       (labels
	   ((,recur ()
	      (let ((,cmd (handler-case
			      (case (sb-ext:with-timeout *timeout*
				      (read-char))
				(#\a :left)
				(#\s :down)
				(#\d :right)
				(#\q :quit)
				(#\space :rotate))
			    (sb-ext:timeout ()
			      nil))))
		    ,@body
		    (,recur))))
	 (,recur)))))


(defun put-mino ()
  (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
    (loop for dx below mw do
	 (loop for dy below mh do
	      (if (aref (mino-shape mino) dx dy)
		  (setf (aref board (+ x0 dx) (+ y0 dy))
			(mino-color mino)))))))

(defun check-going-down ()
  (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
    (print y0)
    (and (< (+ y0 mh) bh)
	 (loop for dx below mw always
	      (loop for dy below mh never
		   (and (aref (mino-shape mino) dx dy)
			(aref board (+ x0 dx) (+ y0 dy 1))))))))
(defun check-going-right ()
  (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
    (and (< (+ x0 mw) bw)
	 (loop for dx below mw always
	      (loop for dy below mh never
		   (and (aref (mino-shape mino) dx dy)
			(aref board (+ x0 dx 1) (+ y0 dy))))))))
(defun check-going-left ()
  (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
    (and (< 0 x0)
	 (loop for dx below mw always
	      (loop for dy below mh never
		   (and (aref (mino-shape mino) dx dy)
			(aref board (+ x0 dx -1) (+ y0 dy))))))))
;; (defun check-rotating-right ()
;;   (with-board-params (x0 y0 mw mh bw bh pos mino board) *game*
;;     (let ((new-mino
;;     (and (< (+ x0 mw) bw)
;; 	 (loop for dx below mw always
;; 	      (loop for dy below mh never
;; 		   (and (aref (mino-shape mino) dx dy)
;; 			(aref board (+ x0 dx 1) (+ y0 dy))))))))

(defmacro game-position ()
  `(cadr (game-mino-and-position *game*)))

(defun go-down ()
  (incf (second (game-position))))
(defun go-right ()
  (incf (first (game-position))))
(defun go-left ()
  (decf (first (game-position))))

(defun progress-game ()
  (with-slots (board next-mino mino-and-position) *game*
    (if mino-and-position
    	(destructuring-bind (mino position) mino-and-position
	  (if (check-going-down)
	      (go-down)
	      (progn (put-mino)
		     (setf mino-and-position nil))))
	(setf mino-and-position
	      (list next-mino (list 3 0))
	      next-mino
	      (random-mino)))))

(defun check-finish (game)
  (or (game-overp game)
      (let ((board (game-board game)))
	(destructuring-bind (w h) (array-dimensions  board)
	  (loop for x below w thereis (aref board x 2))))))

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
  (handler-case
      (progn
	(with-game-lock
	  (unless (check-finish *game*)
	    (progress-game)
	    (show-game)
	    (sb-ext:schedule-timer *timer* *timeout*))))
    (sb-ext:timeout ()
      (sb-ext:schedule-timer *timer* *timeout*))
    ))  

(defun main ()
  (setf *game* (make-game :board
			  (make-array '(8 10) :initial-element nil)
			  :next-mino (random-mino)
			  :mino-and-position nil
			  :lock (sb-thread:make-mutex))
	*timer* (sb-ext:make-timer #'progress-game-timer-func))
  (progn
    (sb-ext:schedule-timer *timer* *timeout*)
    (let ((board (game-board *game*))
	  (mino  (first (game-mino-and-position *game*)))
	  (position (second (game-mino-and-position *game*))))

      (unwind-protect
	 (with-raw-mode
	   (loop
		(let ((cmdchar (get-command-char)))
		  (with-game-lock
		    (case cmdchar
		      (#\d (and (check-going-right)
				(go-right)))
		      (#\a (and (check-going-left)
				(go-left)))
		      (#\s (and (check-going-down)
				(go-down)))
		      ((#\q #\etx) (setf (game-overp *game*) t)))))
	      (when (check-finish *game*)
		(sb-ext:unschedule-timer *timer*)
		(set-pos +LEFT+
			 (+ +TOP+ (array-dimension (game-board *game*) 1) 2))
		(format t "game over~%")
		(finish-output)
		(return-from main (values)))))
	(setf (game-overp *game*) t))
      )))

(defun getch ()
  ; thin wrapper of charms:get-char
  (charms:get-char charms:*standard-window* :ignore-error t))

(defun get-command ()
  "get character, or arrow key (from ANSI escape sequence)"
  (block nil
    (let (c1 c2 c3)
      (setf c1 (getch))
      (when (not (eql c1 #\esc))
	(return c1))
      (setf c2 (getch))
      (when (null c2)
	(return #\esc))
      (setf c3 (getch))
      (case c3
	(#\A :up)
	(#\B :down)
	(#\C :right)
	(#\D :left)
	(t :unknown)))))

(defun fuga ()
  (charms:with-curses ()
    (charms:enable-raw-input)
    (charms:disable-echoing)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    (print
     (loop for i below 5
	collect (loop for a = (get-command)
		   until a
		   ;; do (sleep 0.5)
		   finally (progn (charms:write-string-at-cursor charms:*standard-window* (format nil "~a" a))
				  (return a))))
     )
    ))

;; (fuga)
(main)
