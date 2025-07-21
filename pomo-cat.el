;;; pomo-cat.el --- Pomodoro timer with cat breaks -*- lexical-binding: t; -*-

(eval-when-compile (require 'posframe nil t))
(require 'popon nil t)

(defgroup pomo-cat nil
  "Pomodoro timer with cat breaks."
  :group 'productivity)

(defcustom pomo-cat-work-duration-seconds 1500  ; 25分
  "Duration of work period in seconds."
  :type 'integer)

(defcustom pomo-cat-break-duration-seconds 300  ; 5分
  "Duration of short break in seconds."
  :type 'integer)

(defcustom pomo-cat-long-break-duration-seconds 900  ; 15分
  "Duration of long break in seconds."
  :type 'integer)

(defcustom pomo-cat-cat-image-path nil
  "Path to cat image for GUI display. If nil, ASCII art will be shown."
  :type '(choice (const nil) file))

(defcustom pomo-cat-cycles-before-long-break 4
  "Number of Pomodoro cycles before a long break."
  :type 'integer)

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

(defun pomo-cat--show-ascii-cat ()
  (cond
   ((and (featurep 'popon) (not (display-graphic-p)))
    (let* ((cat-text (if (stringp pomo-cat--ascii-cat)
                         pomo-cat--ascii-cat
                       (format "%s" pomo-cat--ascii-cat)))
           (frame-width (frame-width))
           (frame-height (frame-height))
           (lines (length (split-string cat-text "\n")))
           (cols (apply #'max (mapcar #'string-width (split-string cat-text "\n"))))
           (x (max 0 (/ (- frame-width cols) 2)))
           (y (max 0 (/ (- frame-height lines) 2))))
      (when (stringp cat-text)
        (setq pomo-cat--popon-instance
              (popon-create cat-text `(,x . ,y))))))
   ((and (featurep 'posframe) (display-graphic-p))
    (posframe-show "*pomo-cat*"
                   :string (if (stringp pomo-cat--ascii-cat)
                               pomo-cat--ascii-cat
                             (format "%s" pomo-cat--ascii-cat))
                   :position (point)
                   :poshandler #'posframe-poshandler-frame-center
                   :background-color "#1e1e1e"
                   :foreground-color "#ffffff"
                   :width 50
                   :height 10))
   (t
    (message "\n%s" (if (stringp pomo-cat--ascii-cat)
                        pomo-cat--ascii-cat
                      (format "%s" pomo-cat--ascii-cat))))))

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
  (if (and (display-graphic-p) pomo-cat-cat-image-path)
      (pomo-cat--show-image)
    (pomo-cat--show-ascii-cat)))

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
