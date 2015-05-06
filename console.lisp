(defpackage console
  (:use :common-lisp)
  (:export :get-command
           :with-color-console
	   :write-at-cursor
	   :write-at
	   :move-to
	   :+default-colors+
	   :clear
	   :refresh))

(in-package :console)

(defparameter *colors* ())
(defparameter *pairs* ())

(defun getch ()
  ; thin wrapper of charms:get-char
  (charms:get-char charms:*standard-window* :ignore-error t))

(defun get-command ()
  "get character, or arrow key (from ANSI escape sequence)"
  (let ((c1 (getch)))
    (when c1
      (case (char-code c1)
	(#.charms/ll:key_left  :left)
	(#.charms/ll:key_right :right)
	(#.charms/ll:key_up    :up)
	(#.charms/ll:key_down  :down)
	(t c1)))))

(alexandria:define-constant +default-colors+
  '(:black :red :green :yellow :blue :magenta :cyan :white) :test #'equal)

(defun init-default-colors ()
  (setf *colors*
	`((:black   . ,charms/ll:color_black)
	  (:red     . ,charms/ll:color_red)
	  (:green   . ,charms/ll:color_green)
	  (:yellow  . ,charms/ll:color_yellow)
	  (:blue    . ,charms/ll:color_blue)
	  (:magenta . ,charms/ll:color_magenta)
	  (:cyan    . ,charms/ll:color_cyan)
	  (:white   . ,charms/ll:color_white))))
(defun init-default-pairs ()
  (setf *pairs* nil))
;; color: 
(defun add-color (name r g b)
  "color: (:name r g b)"
  (assert (not (assoc name *colors*)))
  (assert (= 0 (charms/ll:init-color (length *colors*) r g b)))
  (push (cons name (length *colors*)) *colors*))

(defun add-pair (name f b)
  (assert (assoc f *colors*))
  (assert (assoc b *colors*))
  (let ((fg (assoc f *colors*))
	(bg (assoc b *colors*)))
    (charms/ll:init-pair (1+ (length *pairs*)) (cdr fg) (cdr bg))
    (push (cons name (1+ (length *pairs*))) *pairs*)))

(defmacro with-color-console (colors color-pairs &body body)
  `(charms:with-curses ()
     (charms/ll:start-color)

     (charms:disable-echoing)
     (charms:enable-non-blocking-mode charms:*standard-window*)
     (charms:enable-raw-input)
     (charms:enable-extra-keys charms:*standard-window*)

     (init-default-colors)
     (init-default-pairs)
     (loop for (name r g b) in ,colors
	do (add-color name r g b))
     (loop for (name f g) in ,color-pairs
	do (add-pair name f g))
     ,@body))

(defun write-at-cursor (value &key (color-pair 0)
				standout underline reverse blink dim bold protect invis altcharset)
  (let* ((pair (if (symbolp color-pair)
		   (charms/ll:color-pair (cdr (assoc color-pair *pairs*)))
		   color-pair))
	 (attr (+ pair
		  (if standout charms/ll:a_standout 0)
		  (if underline charms/ll:a_underline 0)
		  (if reverse charms/ll:a_reverse 0)
		  (if blink charms/ll:a_blink 0)
		  (if dim charms/ll:a_dim 0)
		  (if bold charms/ll:a_bold 0)
		  (if protect charms/ll:a_protect 0)
		  (if invis charms/ll:a_invis 0)
		  (if altcharset charms/ll:a_altcharset 0))))
    (charms/ll:attron attr)
    (charms:write-string-at-cursor charms:*standard-window* (format nil "~a" value))
    (charms/ll:attroff attr)))

(defun write-at (value x y &key (color-pair 0)
				standout underline reverse blink dim bold protect invis altcharset)
  (let* ((pair (if (symbolp color-pair)
		   (charms/ll:color-pair (cdr (assoc color-pair *pairs*)))
		   color-pair))
	 (attr (+ pair
		  (if standout charms/ll:a_standout 0)
		  (if underline charms/ll:a_underline 0)
		  (if reverse charms/ll:a_reverse 0)
		  (if blink charms/ll:a_blink 0)
		  (if dim charms/ll:a_dim 0)
		  (if bold charms/ll:a_bold 0)
		  (if protect charms/ll:a_protect 0)
		  (if invis charms/ll:a_invis 0)
		  (if altcharset charms/ll:a_altcharset 0))))
    (charms/ll:attron attr)
    (charms:write-string-at-point charms:*standard-window* (format nil "~a" value) x y)
    (charms/ll:attroff attr)))

(defun piyo ()
  (let* ((pairs  '((:bluefg :blue :black)
		   (:yellowfg :yellow :black))))
    (with-color-console nil pairs

      (list
       (loop for i below 5
	  collect (loop for a = (get-command)
		     until a
		     finally (progn
			       (write-at-cursor a :color-pair :bluefg)
			       (write-at-cursor " ")
			       (write-at-cursor a :color-pair :bluefg :bold t)
			       (write-at-cursor " ")
			       (write-at-cursor a :color-pair :bluefg :bold t :altcharset t)
			       (write-at-cursor " ")
			       (write-at-cursor a :color-pair :yellowfg :underline t :reverse t :standout t)
			       (write-at-cursor " ")
			       (return a))))
       'piyo (charms/ll:can-change-color)
       :pairs *pairs* pairs)
       )))

;; (print (piyo))


(defun move-to (x y)
  (charms:move-cursor charms:*standard-window* x y))

(defun clear ()
  (charms:clear-window charms:*standard-window* :force-repaint t))

(defun refresh ()
  (charms:refresh-window charms:*standard-window*))
