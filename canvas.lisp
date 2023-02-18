(in-package #:calm)

;;
;; CALM version check
;;

(let ((required-version "0.0.30")
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
(setf *calm-window-icon* "assets/icon.png")

(setf *calm-window-title*
      (if (string= (uiop:getenv "BUILD_TRIAL") "yah")
          "Focalizzare - Free Trial"
          "Focalizzare"))

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

(defun reset-timer ()
  (format t "RESETING.... ~%")
  (format t "ROUND: ~A BREAK: ~A~%" *round* *breaking*)
  (if *breaking*
      (if (= *round* 3)
          (setf *breaking-timer* (* 20 60))
          (setf *breaking-timer* (* 5 60)))
      (setf *timer* (* 25 60))))

(defun reset-round ()
  (setf *round* 0))

(defun record-timer ()
  (when (and (not *pause*) (= *last-second* 0))
    (setf *last-second* (get-universal-time))))

(defun stop-timer ()
  (setf *pause* t)
  (setf *last-second* 0))

(defun incf-timer ()
  (when (not *pause*)
    (let* ((curr-second (get-universal-time))
           (duration (- curr-second *last-second*))
           (drawp nil))
      ;; when more than 1 second passed
      (when ( >= duration 1)
        (setf drawp t)
        (setf *last-second* curr-second)
        (if *breaking*
            (progn (incf *breaking-timer* (* -1 duration)) (when (< *breaking-timer* 0) (setf *breaking-timer* 0)))
            (progn (incf *timer* (* -1 duration)) (when (< *timer* 0) (setf *timer* 0)))))
      ;; when time is up
      (when (<= (if *breaking* *breaking-timer* *timer*) 0)
        (setf drawp t)
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
        (when (not (u:audio-is-playing))
          (if *breaking*
              (u:play-music "assets/bell.ogg")
              (u:play-music "assets/bell-break.ogg"))))
      drawp)))

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

;;
;; events handling
;;

(defun on-keyup (key)
  (cond
    ((eq key :SCANCODE-SPACE)
     (progn (setf *pause* (not *pause*)) (when *pause* (stop-timer))))
    ((eq key :SCANCODE-SLASH)
     (setf *show-help* t))
    ((eq key :SCANCODE-ESCAPE)
     (setf *show-help* nil))
    ((eq key :SCANCODE-B)
     (browse "https://vitovan.com/focalizzare"))
    (t (format t "~%KEY PRESSED: ~A~%" key))))


(defun is-in-rectangle (x y rx ry rwidth rheight)
  "to test if the mouse is inside an area, for testing CLICK and HOVER event"
  (and
   (> x rx) (> y ry)
   (< x (+ rx rwidth))
   (< y (+ ry rheight))))

(defun on-mousebuttonup (&key button x y clicks)
  ;; clicking pause or resume button
  (when (is-in-rectangle x y 53 135 28 33)
    (setf *pause* (not *pause*))
    (when *pause* (stop-timer)))

  ;; clicking reset button
  (when (is-in-rectangle x y 215 135 30 30)
    (setf *breaking* nil)
    (stop-timer)
    (setf *round* 0)
    (stop-timer)
    (reset-timer)))

(defun think ()
  "this method is called every `*calm-delay*' milliseconds, no matter what."
  (record-timer)
  (incf-timer))

(defun draw ()
  "this method is called every `*calm-delay*' milliseconds,
only when the window is not hidden or minimized."

  (c:select-font-face "Arial" :normal :normal)

  (when *show-help*
    (c:set-font-size 18)
    (c:move-to 20 30)
    (apply #'c:set-source-rgb *button-color*)
    (c:show-text "Press:")
    (c:move-to 30 60)
    (c:show-text "<?>")
    (c:move-to 120 60)
    (c:show-text "to show this help")
    (c:move-to 30 90)
    (c:show-text "<ESC>")
    (c:move-to 120 90)
    (c:show-text "to close this help")
    (c:move-to 30 120)
    (c:show-text "<SPACE>")
    (c:move-to 120 120)
    (c:show-text "to start or pause")
    (c:move-to 30 150)
    (c:show-text "<B>")
    (c:move-to 120 150)
    (c:show-text "to open the website")

    (c:move-to 50 180)
    (apply #'c:set-source-rgb *button-color-hover*)
    (c:show-text "    May you be happy.")
    (return-from draw 42))


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
  (draw-reset-button))
