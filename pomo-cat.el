;;; pomo-cat.el --- Pomodoro timer with cat-themed breaks -*- lexical-binding: t; -*-

;; Author: Nobuyuki Kamimoto
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (popon "0.1") (posframe "1.1.1"))
;; Keywords: convenience, tools, calendar
;; URL: https://github.com/kn66/pomo-cat.el
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; pomo-cat.el is a simple Pomodoro timer with cat-themed break messages.
;; During breaks, it shows an ASCII cat or an optional cat image using
;; either `popon` (TTY) or `posframe` (GUI).  It supports configurable
;; durations and cycles, making it easy to adopt for your workflow.

;;; Code:

(require 'posframe nil t)
(require 'popon nil t)

(defgroup pomo-cat nil
  "A Pomodoro timer that displays a cat during breaks."
  :group 'productivity)

(defcustom pomo-cat-work-duration-seconds (* 60 25)
  "Duration of each Pomodoro work session in seconds."
  :type 'integer)

(defcustom pomo-cat-break-duration-seconds (* 60 5)
  "Duration of a short break in seconds."
  :type 'integer)

(defcustom pomo-cat-long-break-duration-seconds (* 60 20)
  "Duration of a long break (after several cycles) in seconds."
  :type 'integer)

(defcustom pomo-cat-delay-break-seconds 60
  "Default delay for unwanted breaks."
  :type 'integer)

(defcustom pomo-cat-cat-image-path nil
  "Path to a cat image (e.g., PNG) to be shown in GUI.  If nil, ASCII art is used."
  :type '(choice (const nil) file))

(defcustom pomo-cat-cycles-before-long-break 4
  "Number of Pomodoro work sessions before a long break."
  :type 'integer)

(defcustom pomo-cat-display-method 'popon
  "Display method for the cat during breaks.

- \\='popon: Uses popon (recommended for terminal).
- \\='posframe: Uses posframe (recommended for GUI; supports optional images)."
  :type
  '(choice
    (const :tag "popon (terminal)" popon)
    (const :tag "posframe (GUI)" posframe)))

(defcustom pomo-cat-ascii-cat "
███████████████████████████
█                         █
█      Take a break       █
█                         █
█         /\\_/\\           █
█        ( o.o )          █
█         > ^ <           █
█                         █
█                         █
███████████████████████████
"
  "ASCII art to show when cat image is not available."
  :type 'string)

(defcustom pomo-cat-get-focus nil
  "Bring Emacs' zeroth frame to the front and in focus.
I.e. to notify the user of a break even if not working in Emacs."
  :type 'boolean)

(defvar pomo-cat--timer nil
  "Internal timer object used for Pomodoro intervals.")

(defvar pomo-cat--cycle-count 0
  "Number of completed Pomodoro work sessions.")

(defvar pomo-cat--current-break-type 'short
  "Current break type: \\='short or \\='long.")

(defvar pomo-cat--popon-instance nil
  "Internal popon instance for text-based cat display.")

(defvar pomo-cat--in-break nil
  "Indicator if currently in a break.")

(defun pomo-cat--clear-cat-display ()
  "Clear the current cat display, whether posframe or popon."
  (when (featurep 'posframe)
    (when (posframe-workable-p)
      (posframe-delete "*pomo-cat*")))
  (when (and (featurep 'popon) pomo-cat--popon-instance)
    (popon-kill pomo-cat--popon-instance)
    (setq pomo-cat--popon-instance nil)))

(defun pomo-cat--theme-colors ()
  "Return a (foreground . background) cons cell based on current theme."
  (let ((fg
         (or (face-attribute 'default :foreground nil t) "#ffffff"))
        (bg
         (or (face-attribute 'default :background nil t) "#000000")))
    (cons fg bg)))

(defun pomo-cat--measure-ascii (text)
  "Return (width . height) of multiline ASCII TEXT."
  (let* ((lines (split-string text "\n"))
         (height (length lines))
         (width (apply #'max 0 (mapcar #'string-width lines))))
    (cons width height)))

(defun pomo-cat--show-ascii-cat ()
  "Display ASCII art of the cat using `popon` or `posframe`."
  (let* ((cat-text
          (if (stringp pomo-cat-ascii-cat)
              pomo-cat-ascii-cat
            (format "%s" pomo-cat-ascii-cat)))
         (size (pomo-cat--measure-ascii cat-text))
         (cols (car size))
         (lines (cdr size)))
    (cond
     ((and (featurep 'popon) (not (display-graphic-p)))
      (let* ((frame-width (frame-width))
             (frame-height (frame-height))
             (x (max 0 (/ (- frame-width cols) 2)))
             (y (max 0 (/ (- frame-height lines) 2))))
        (setq pomo-cat--popon-instance
              (popon-create cat-text `(,x . ,y)))))
     ((and (featurep 'posframe) (display-graphic-p))
      (let* ((colors (pomo-cat--theme-colors))
             (fg (car colors))
             (bg (cdr colors)))
        (posframe-show
         "*pomo-cat*"
         :string cat-text
         :position (point)
         :poshandler #'posframe-poshandler-frame-center
         :background-color bg
         :foreground-color fg
         :width cols
         :height lines)))
     (t
      (message "\n%s" cat-text)))))

(defun pomo-cat--show-image ()
  "Display the configured cat image using posframe, if available."
  (when (and (featurep 'posframe)
             (display-graphic-p)
             (stringp pomo-cat-cat-image-path)
             (file-exists-p pomo-cat-cat-image-path))
    (let* ((img (create-image pomo-cat-cat-image-path))
           (width (car (image-size img t)))
           (height (cdr (image-size img t)))
           (char-width (frame-char-width))
           (char-height (frame-char-height))
           (cols (ceiling (/ (float width) char-width)))
           (lines (ceiling (/ (float height) char-height))))
      (posframe-show
       "*pomo-cat*"
       :string ""
       :poshandler #'posframe-poshandler-frame-center
       :width cols
       :height lines)
      (with-current-buffer "*pomo-cat*"
        (erase-buffer)
        (insert-image img)))))

(defun pomo-cat--show-cat ()
  "Show a cat using the selected `pomo-cat-display-method`."
  (cond
   ((and (eq pomo-cat-display-method 'posframe)
         (display-graphic-p)
         (stringp pomo-cat-cat-image-path)
         (file-exists-p pomo-cat-cat-image-path))
    (pomo-cat--show-image))

   ((and (eq pomo-cat-display-method 'posframe) (display-graphic-p))
    (posframe-show
     "*pomo-cat*"
     :string
     (if (stringp pomo-cat-ascii-cat)
         pomo-cat-ascii-cat
       (format "%s" pomo-cat-ascii-cat))
     :position (point)
     :poshandler #'posframe-poshandler-frame-center))

   (t
    (pomo-cat--show-ascii-cat))))

(defun pomo-cat--start-break ()
  "Begin a short or long break and show cat display."
  (setq
   pomo-cat--current-break-type
   (if (eq
        (% pomo-cat--cycle-count pomo-cat-cycles-before-long-break) 0)
       'long
     'short)
   pomo-cat--in-break t)
  (let ((duration
         (if (eq pomo-cat--current-break-type 'long)
             pomo-cat-long-break-duration-seconds
           pomo-cat-break-duration-seconds)))
    (message "Break started! (%s break)"
             (symbol-name pomo-cat--current-break-type))
    (pomo-cat--show-cat)
    (when pomo-cat-get-focus
      (other-frame 0))
    (setq pomo-cat--timer
          (run-at-time duration nil #'pomo-cat--start-work))))

(defun pomo-cat--start-work ()
  "Begin a new Pomodoro work session."
  (pomo-cat--clear-cat-display)
  (setq pomo-cat--cycle-count (1+ pomo-cat--cycle-count))
  (message "Pomodoro work #%d started!" pomo-cat--cycle-count)
  (setq
   pomo-cat--timer
   (run-at-time
    pomo-cat-work-duration-seconds nil #'pomo-cat--start-break)
   pomo-cat--in-break nil))

;;;###autoload
(defun pomo-cat-start ()
  "Start the Pomodoro timer."
  (interactive)
  ;; Stop any existing timer and clear display
  (when pomo-cat--timer
    (cancel-timer pomo-cat--timer))
  (pomo-cat--clear-cat-display)
  (setq pomo-cat--cycle-count 0)
  (pomo-cat--start-work))

;;;###autoload
(defun pomo-cat-stop ()
  "Stop the Pomodoro timer and clear cat display."
  (interactive)
  (when pomo-cat--timer
    (cancel-timer pomo-cat--timer))
  (setq pomo-cat--timer nil
        pomo-cat--cycle-count 0
        pomo-cat--in-break nil
        pomo-cat--current-break-type 'short)
  (pomo-cat--clear-cat-display)
  (message "Pomodoro stopped."))

;;;###autoload
(defun pomo-cat-delay-break (&optional seconds)
  "Delay the current break for SECONDS."
  (interactive "P")
  (when pomo-cat--in-break
    (pomo-cat--clear-cat-display)
    (cancel-timer pomo-cat--timer)
    (let ((delay (or seconds pomo-cat-delay-break-seconds)))
      (setq pomo-cat--timer
            (run-at-time delay nil #'pomo-cat--start-break))
      (message "Break delayed %ss." delay))))

;;;###autoload
(defun pomo-cat-stop-break ()
  "Simply stop the current break period as if it has finished."
  (interactive)
  (when pomo-cat--in-break
    (pomo-cat--clear-cat-display)
    (cancel-timer pomo-cat--timer)
    (pomo-cat--start-work)))

;; Attempt to auto-set image path if cat.png is bundled with the file
(unless pomo-cat-cat-image-path
  (when load-file-name
    (setq pomo-cat-cat-image-path
          (expand-file-name "cat.png"
                            (file-name-directory load-file-name)))))

(provide 'pomo-cat)

;;; pomo-cat.el ends here
