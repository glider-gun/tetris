# Description

+ tetris that works at terminal
+ common lisp (SBCL only)

# Usage

+ move minos with arrow keys
+ rotete minos with up arrow key or space key
+ exit with q / ESC / Ctrl+C

# How to launch

sbcl and quicklisp is required.
clone this repository to ~/quicklisp/local-project/

Try
```
$ sbcl --eval '(ql:quickload :tetris :silent t)' --eval '(tetris:main)' --quit
```
to launch. Or
```
$ sbcl --eval '(ql:quickload :tetris :silent t)' --eval '(tetris:make-tetris-command)' --quit
```
to make executable file and do
```
$ ./tetris
```

# Todo
+ tidier code
