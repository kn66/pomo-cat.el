;;; pomo-cat.el --- Pomodoro timer with cat breaks -*- lexical-binding: t; -*-

(eval-when-compile (require 'posframe nil t))
(require 'popon nil t)

(defgroup pomo-cat nil
  "Pomodoro timer with cat breaks."
  :group 'productivity)

(defcustom pomo-cat-work-duration-seconds (* 60 25)
  "Duration of work period in seconds."
  :type 'integer)

(defcustom pomo-cat-break-duration-seconds (* 60 5)
  "Duration of short break in seconds."
  :type 'integer)

(defcustom pomo-cat-long-break-duration-seconds (* 60 20)
  "Duration of long break in seconds."
  :type 'integer)

(defcustom pomo-cat-cat-image-path nil
  "Path to cat image for GUI display. If nil, ASCII art will be shown."
  :type '(choice (const nil) file))

(defcustom pomo-cat-cycles-before-long-break 4
  "Number of Pomodoro cycles before a long break."
  :type 'integer)

(defcustom pomo-cat-display-method 'popon
  "Method to display the cat during breaks.
- 'popon: Use popon (TTY).
- 'posframe: Use posframe (GUI + optional image)."
  :type '(choice (const :tag "popon (text display)" popon)
                 (const :tag "posframe (GUI with optional image)" posframe))
  :group 'pomo-cat)

(defvar pomo-cat--timer nil)
(defvar pomo-cat--cycle-count 0)
(defvar pomo-cat--current-break-type 'short)

(defconst pomo-cat--ascii-cat
  "
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
  "ASCII cat shown during breaks.")

(defvar pomo-cat--popon-instance nil)

(defun pomo-cat--clear-cat-display ()
  "Hide the cat display."
  (when (featurep 'posframe)
    (when (posframe-workable-p)
      (posframe-delete "*pomo-cat*")))
  (when (and (featurep 'popon) pomo-cat--popon-instance)
    (popon-kill pomo-cat--popon-instance)
    (setq pomo-cat--popon-instance nil)))

(defun pomo-cat--theme-colors ()
  "Return a cons of (foreground . background) from current theme, with fallback."
  (let ((fg (or (face-attribute 'default :foreground nil t) "#ffffff"))
        (bg (or (face-attribute 'default :background nil t) "#000000")))
    (cons fg bg)))

(defun pomo-cat--measure-ascii (text)
  "Measure width and height (columns and lines) of ASCII TEXT."
  (let* ((lines (split-string text "\n"))
         (height (length lines))
         (width (apply #'max 0 (mapcar #'string-width lines))))
    (cons width height)))

(defun pomo-cat--show-ascii-cat ()
  (let* ((cat-text (if (stringp pomo-cat-ascii-cat)
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
        (posframe-show "*pomo-cat*"
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
      (posframe-show "*pomo-cat*"
                     :string ""
                     :poshandler #'posframe-poshandler-frame-center
                     :width cols
                     :height lines)
      (with-current-buffer "*pomo-cat*"
        (erase-buffer)
        (insert-image img)))))

(defun pomo-cat--show-cat ()
  "Show the cat display according to `pomo-cat-display-method`."
  (cond
   ;; posframe + image
   ((and (eq pomo-cat-display-method 'posframe)
         (display-graphic-p)
         (stringp pomo-cat-cat-image-path)
         (file-exists-p pomo-cat-cat-image-path))
    (pomo-cat--show-image))

   ;; posframe + ascii fallback
   ((and (eq pomo-cat-display-method 'posframe)
         (display-graphic-p))
    (posframe-show "*pomo-cat*"
                   :string (if (stringp pomo-cat-ascii-cat)
                               pomo-cat-ascii-cat
                             (format "%s" pomo-cat-ascii-cat))
                   :position (point)
                   :poshandler #'posframe-poshandler-frame-center))

   ;; popon fallback (default)
   (t
    (pomo-cat--show-ascii-cat))))

(defun pomo-cat--start-break ()
  "Start a break (short or long) depending on cycle count."
  (setq pomo-cat--current-break-type
        (if (eq (% pomo-cat--cycle-count pomo-cat-cycles-before-long-break) 0)
            'long
          'short))
  (let ((duration (if (eq pomo-cat--current-break-type 'long)
                      pomo-cat-long-break-duration-seconds
                    pomo-cat-break-duration-seconds)))
    (message "Break started! (%s break)" (symbol-name pomo-cat--current-break-type))
    (pomo-cat--show-cat)
    (setq pomo-cat--timer
          (run-at-time duration nil #'pomo-cat--start-work))))

(defun pomo-cat--start-work ()
  "Start a Pomodoro work session."
  (pomo-cat--clear-cat-display)
  (setq pomo-cat--cycle-count (1+ pomo-cat--cycle-count))
  (message "Pomodoro work #%d started!" pomo-cat--cycle-count)
  (setq pomo-cat--timer
        (run-at-time pomo-cat-work-duration-seconds nil #'pomo-cat--start-break)))

;;;###autoload
(defun pomo-cat-start ()
  "Start the Pomodoro timer."
  (interactive)
  (setq pomo-cat--cycle-count 0)
  (pomo-cat--start-work))

;;;###autoload
(defun pomo-cat-stop ()
  "Stop the Pomodoro timer and hide any cat display."
  (interactive)
  (when pomo-cat--timer
    (cancel-timer pomo-cat--timer))
  (setq pomo-cat--timer nil)
  (pomo-cat--clear-cat-display)
  (message "Pomodoro stopped."))

(unless pomo-cat-cat-image-path
  (when load-file-name
    (setq pomo-cat-cat-image-path
          (expand-file-name "cat.png" (file-name-directory load-file-name)))))

(provide 'pomo-cat)

;;; pomo-cat.el ends here
