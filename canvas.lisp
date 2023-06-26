;;
;; No, shit! you are looking at the source code!
;; This is absolutely a piece of shit and might cause physical damage to your eyes.
;;
;; I have been programming for many years,
;; but I have never managed to be professional in any programming language.
;;
;; Especially Lisp.
;;
;; I like Lisp a lot, but it is such a pain to distribute software written in Lisp,
;; and so few people are working on this language (compare to JavaScript? Maybe this is a good thing).
;;
;; So I tried to do some shit work to make Lisp software (SDL2 alike) distribution easier,
;; nothing high-tech, they are just a bunch of shitty bash scripts, some kindergarten C and a little messy Lisp.
;;
;; After the shit work is done, we could generate a cross-platform (Linux / macOS / Windows) software
;; with a single Lisp file.
;;
;; Like this one Focalizzare.
;;
;; All we need is to write a file named `canvas.lisp', and then leave the rest to `CALM' - a shit work bearer.
;;
;; For more about `CALM', please check: https://github.com/VitoVan/calm/
;; Of course, it lacks documentation by the time of now (Feb 2023),
;; and it is just a mix of cl-cairo2, cl-sdl2 and some other things.
;;
;; So, that's it. I will leave you to the code, and may you enjoy Lisp.
;;


(in-package #:calm)

;;
;; CALM version check
;;

(let ((required-version "1.1.0")
      (calm-version (slot-value (asdf:find-system 'calm) 'asdf:version)))
  (when (uiop:version< calm-version required-version)
    (format t "Sorry, this is built on CALM ~A, older version (current: ~A) of CALM won't work.~%" required-version calm-version)
    (uiop:quit 42)))

;; the swank server is for debugging, for usage please check
;; Emacs:
;;        https://slime.common-lisp.dev/
;; Visual Studio Code
;;        https://lispcookbook.github.io/cl-cookbook/vscode-alive.html

(unless (str:starts-with? "dist" (uiop:getenv "CALM_CMD"))
  (swank:create-server))

;;
;; by default, the screensaver is disabled,
;; if you want to enable screensaver,
;; please uncomment the following line
;;
;; (setf (uiop:getenv "SDL_VIDEO_ALLOW_SCREENSAVER") "1")

;; setting window properties, for more of this, please check
;;      https://github.com/VitoVan/calm/blob/main/src/config.lisp

(setf *calm-window-width* 300)
(setf *calm-window-height* 200)

(setf *calm-window-title* "Focalizzare")

;;
;; custom parameters
;;

(defparameter *bg-color* '(1 1 1))
(defparameter *fg-color* '(0 0 0))
(defparameter *fg-color-pause* (list (/ 142 255) (/ 142 255) (/ 142 255)))
(defparameter *button-color* '(0 0.35 0.59))
(defparameter *button-color-hover* '(0.89 0.12 0.17))

(defparameter *last-second* 0)

(defparameter *timer* (* 25 60))
(defparameter *round* 0)
(defparameter *pause* t)

(defparameter *breaking* nil)
(defparameter *breaking-timer* (* 5 60))

(defparameter *hovering-pause-resume-button* nil)
(defparameter *hovering-reset-button* nil)

(defparameter *show-help* nil)


;;
;; custom logic functions
;;

(defun stop-timer ()
  (setf *pause* t)
  (setf *last-second* 0))

(defun reset-timer ()
  "this resets the timer, not the whole rounds, for that please check `reset-all'"
  (format t "RESETING.... ~%")
  (format t "ROUND: ~A BREAK: ~A~%" *round* *breaking*)
  (if *breaking*
      (if (= *round* 3)
          (setf *breaking-timer* (* 20 60))
          (setf *breaking-timer* (* 5 60)))
      (setf *timer* (* 25 60))))

(defun reset-all ()
  "reset everything"
  (setf *breaking* nil)
  (stop-timer)
  (setf *round* 0)
  (stop-timer)
  (reset-timer))

(defun reset-round ()
  (setf *round* 0))

(defun record-timer ()
  (when (and (not *pause*) (= *last-second* 0))
    (setf *last-second* (get-universal-time))))

(defun incf-timer ()
  (when (not *pause*)
    (c:open-audio-if-not-yet)
    (let* ((curr-second (get-universal-time))
           (duration (- curr-second *last-second*)))
      ;; when more than 1 second passed
      (when ( >= duration 1)
        (setf *last-second* curr-second)
        (if *breaking*
            (progn (incf *breaking-timer* (* -1 duration)) (when (< *breaking-timer* 0) (setf *breaking-timer* 0)))
            (progn (incf *timer* (* -1 duration)) (when (< *timer* 0) (setf *timer* 0))))
        (setf *calm-redraw* t))
      ;; when time is up
      (when (<= (if *breaking* *breaking-timer* *timer*) 0)
        (if *breaking*
            ;; if it's breaking, then finish the break
            ;; and increase the round
            (progn
              (setf *breaking* nil)
              (if (< *round* 3)
                  (incf *round*)
                  (setf *round* 0)))
            ;; if it's not the breaking, then break
            (setf *breaking* t))
        ;; either way, we need to reset the timer and pause
        (reset-timer)
        (stop-timer)
        (when (zerop (sdl2-mixer:playing -1))
          (if *breaking*
              (c:play-music "assets/bell.ogg")
              (c:play-music "assets/bell-break.ogg")))))))

;;
;; custom drawing functions
;;

(defun draw-circle (x y radius)
  "draw the dot (round indicator)"
  (c:new-sub-path)
  (c:arc x y radius 0 (* pi 2)))

(defun draw-resume-button ()
  (c:set-line-width 3)
  (if *hovering-pause-resume-button*
      (apply #'c:set-source-rgb *button-color-hover*)
      (apply #'c:set-source-rgb *button-color*))
  (c:move-to 55 138)
  (c:rel-line-to 20 14)
  (c:rel-line-to -20 14)
  (c:rel-line-to 0 -20)
  (c:close-path)
  ;; (c:rectangle 53 135 28 33)
  (c:stroke))

(defun draw-pause-button ()
  (c:set-line-width 3)
  (if *hovering-pause-resume-button*
      (apply #'c:set-source-rgb *button-color-hover*)
      (apply #'c:set-source-rgb *button-color*))
  (c:move-to 61 140)
  (c:rel-line-to 0 25)
  (c:rel-move-to 10 -25)
  (c:rel-line-to 0 25)
  (c:close-path)
  (c:stroke))

(defun draw-reset-button ()
  (c:set-line-width 3)
  (c:set-line-cap :round)
  (if *hovering-reset-button*
      (apply #'c:set-source-rgb *button-color-hover*)
      (apply #'c:set-source-rgb *button-color*))
  (c:new-sub-path)
  (c:arc-negative 230 150 10 (* pi 2.8) (* pi 1.05))
  (c:rel-move-to -2 0)
  (c:rel-line-to 10 0)
  (c:rel-move-to -10 0)
  (c:rel-line-to 0 -10)
  (c:stroke))

(defun browse (url)
  "open `url' in the default browser"
  #+darwin
  (uiop:run-program (str:concat "open " url))
  #+linux
  (uiop:run-program (str:concat "xdg-open " url))
  #+win32
  (uiop:run-program (str:concat "start " url)))

(defun window-is-on-top ()
  (member :always-on-top (sdl2:get-window-flags *calm-window*)))

;;
;; events handling
;;

(defun on-keyup (key)
  (cond
    ((c:keq key :SCANCODE-T)
     (sdl2-ffi.functions:sdl-set-window-always-on-top
      *calm-window*
      (if (window-is-on-top) sdl2-ffi:+false+ sdl2-ffi:+true+)))
    ((c:keq key :SCANCODE-R)
     (reset-all))
    ((c:keq key :SCANCODE-SPACE)
     (progn (setf *pause* (not *pause*)) (when *pause* (stop-timer))))
    ((c:keq key :SCANCODE-SLASH)
     (setf *show-help* t))
    ((c:keq key :SCANCODE-ESCAPE :SCANCODE-Q)
     (setf *show-help* nil))
    ((c:keq key :SCANCODE-B)
     (browse "https://vitovan.com/focalizzare"))
    (t (format t "~%KEY PRESSED: ~A~%" key))))


(defun is-in-rectangle (x y rx ry rwidth rheight)
  "to test if the mouse is inside an area, for testing CLICK and HOVER event"
  (and
   (> x rx) (> y ry)
   (< x (+ rx rwidth))
   (< y (+ ry rheight))))

(defun on-mousebuttonup (&key button x y clicks)
  "clicking pause or resume button"
  (declare (ignore button clicks))
  (when (is-in-rectangle x y 53 135 28 33)
    (setf *pause* (not *pause*))
    (when *pause* (stop-timer)))

  ;; clicking reset button
  (when (is-in-rectangle x y 215 135 30 30)
    (reset-all)))

(defun think ()
  "this method is called every `*calm-delay*' milliseconds, no matter what."
  (record-timer)
  (incf-timer))

(defun draw-forever ()
  "this method is called every `*calm-delay*' milliseconds,
only when the window is not hidden or minimized."

  (c:select-font-face "Arial" :normal :normal)

  (when *show-help*
    (c:select-font-face "Courier" :normal :normal)
    (c:set-font-size 18)
    (apply #'c:set-source-rgb *button-color*)
    (c:move-to 30 40)
    (c:show-text "esc")
    (c:move-to 120 40)
    (c:show-text "close help")
    (c:move-to 30 70)
    (c:show-text "t")
    (c:move-to 120 70)
    (c:show-text "toggle on top")
    (c:move-to 30 100)
    (c:show-text "r")
    (c:move-to 120 100)
    (c:show-text "reset")
    (c:move-to 30 130)
    (c:show-text "space")
    (c:move-to 120 130)
    (c:show-text "start / pause")
    (c:move-to 30 160)
    (c:show-text "b")
    (c:move-to 120 160)
    (c:show-text "open homepage")

    (c:move-to 50 180)
    (apply #'c:set-source-rgb *button-color-hover*)
    (return-from draw-forever 42))


  (apply #'c:set-source-rgb *bg-color*)
  (c:paint)
  (if (or *pause* *breaking*)
      (apply #'c:set-source-rgb *fg-color-pause*)
      (apply #'c:set-source-rgb *fg-color*))

  (c:move-to 22 100)
  (c:set-font-size 100)

  (let* ((time-string
           (format nil
                   "~2,'0d:~2,'0d"
                   (floor (/ *timer* 60)) (mod *timer* 60)))
         (time-chars
           (coerce time-string
                   'list)))

    (loop for x in time-chars
          for c = (string x)
          do (progn
               (when (equal c ":")
                 (cairo:rel-move-to 0 -8))
               (c:show-text c)
               (when (equal c ":")
                 (c:rel-move-to 0 8))
               (c:rel-move-to 2 0))))

  (dotimes (i 4)
    (let ((x (+ 120 (* i 20)))
          (y 150)
          (radius 5))
      (draw-circle x y radius)
      (if (<= i *round*)
          (if (and (= i *round*) (not (= *timer* 0)))
              ;; if this is the current round indicator,
              ;; and this round is not finished
              ;; then update the status to 'half filled circle' or 'stroked circle'
              (if (< *timer* (* 25 60))
                  ;; if it has started, then fill half cirle
                  (progn
                    (c:stroke)
                    (c:new-sub-path)
                    (c:arc x y radius (* pi 0.5) (* pi 1.5))
                    (c:fill-path))
                  ;; if the timer hasn't started yet, stroke
                  (c:stroke))
              ;; if this is the previous round(s),
              ;; or the current *timer* is 0 (finished)
              ;; then fill the indicator
              (c:fill-path))
          (c:stroke))))
  (when *breaking*
    (c:set-source-rgba 0 0 0 1)
    (c:move-to 240 25)
    (c:set-font-size 20)
    (c:show-text
     (format nil "~2,'0d:~2,'0d" (floor (/ *breaking-timer* 60)) (mod *breaking-timer* 60))))

  ;; test the hover states of buttons
  (setf *hovering-pause-resume-button* (is-in-rectangle *calm-state-mouse-x* *calm-state-mouse-y* 53 135 28 33))
  (setf *hovering-reset-button* (is-in-rectangle *calm-state-mouse-x* *calm-state-mouse-y* 215 135 30 30))

  (if *pause*
      (draw-resume-button)
      (draw-pause-button))
  (draw-reset-button)

  (setf *calm-redraw* nil))
