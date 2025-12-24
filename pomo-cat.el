;;; pomo-cat.el --- Pomodoro timer with cat-themed breaks -*- lexical-binding: t; -*-

;; Author: Nobuyuki Kamimoto
;; Version: 1.2
;; Package-Requires: ((emacs "27.1") (popon "0.13") (posframe "1.1.1"))
;; Keywords: convenience, tools, calendar
;; URL: https://github.com/kn66/pomo-cat.el
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; pomo-cat.el is a simple Pomodoro timer with cat-themed break messages.
;; During breaks, it shows an ASCII cat or an optional cat image using
;; `popon` (terminal) or `posframe` (GUI), or in a dedicated frame
;; (independent window).  It supports configurable durations and cycles,
;; making it easy to adopt for your workflow.

;;; Code:

(require 'posframe nil t)
(require 'popon nil t)

;; Silence byte-compiler warnings for optional dependencies
(declare-function posframe-workable-p "posframe")
(declare-function posframe-show "posframe")
(declare-function posframe-delete "posframe")
(declare-function posframe-poshandler-frame-center "posframe")
(declare-function popon-create "popon")
(declare-function popon-kill "popon")

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
  "Path to a cat image (e.g., PNG) to be shown in GUI.  If nil, ASCII art is used.

A sample image `cat.png' is bundled with this package.

Example configuration:
  (setq pomo-cat-cat-image-path
        (expand-file-name \"cat.png\"
                          (file-name-directory (locate-library \"pomo-cat\"))))"
  :type '(choice (const nil) file))

(defcustom pomo-cat-cycles-before-long-break 4
  "Number of Pomodoro work sessions before a long break."
  :type 'integer)

(defcustom pomo-cat-use-dedicated-frame nil
  "Use dedicated frame for break display.
When non-nil and in GUI environment, displays break notification
in a separate independent frame.
When nil, uses posframe (if image is configured) or popon (ASCII art)."
  :type 'boolean)

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

;;; Internal State Management
;;
;; All internal state is consolidated into `pomo-cat--state` plist.
;; This provides:
;; - Atomic state reset via `pomo-cat--reset-state`
;; - Clear state inspection for debugging
;; - Reduced global variable pollution

(defvar pomo-cat--state nil
  "Internal state plist for pomo-cat.
Keys:
  :timer - Timer object for Pomodoro intervals
  :cycle-count - Number of completed work sessions
  :break-type - Current break type (\\='short or \\='long)
  :in-break - Whether currently in a break
  :popon-instance - Popon display instance
  :dedicated-frame - Dedicated frame instance")

(defun pomo-cat--state-get (key)
  "Get value for KEY from internal state."
  (plist-get pomo-cat--state key))

(defun pomo-cat--state-set (key value)
  "Set KEY to VALUE in internal state."
  (setq pomo-cat--state (plist-put pomo-cat--state key value)))

(defun pomo-cat--reset-state ()
  "Reset all internal state to initial values."
  (let ((timer (pomo-cat--state-get :timer)))
    (when timer
      (cancel-timer timer)))
  (setq pomo-cat--state
        '(:timer
          nil
          :cycle-count 0
          :break-type short
          :in-break nil
          :popon-instance nil
          :dedicated-frame nil)))

;;; Validation Functions
;;
;; Input validation to ensure configuration values are sensible.

(defun pomo-cat--validate-positive-integer (value name)
  "Validate that VALUE is a positive integer for config NAME.
Returns VALUE if valid, otherwise returns a sensible default and warns."
  (cond
   ((and (integerp value) (> value 0))
    value)
   ((and (numberp value) (> value 0))
    (let ((rounded (round value)))
      (message "pomo-cat: %s should be integer, using %d"
               name
               rounded)
      rounded))
   (t
    (message "pomo-cat: Invalid %s (%s), using default 60" name value)
    60)))

(defun pomo-cat--get-work-duration ()
  "Get validated work duration in seconds."
  (pomo-cat--validate-positive-integer
   pomo-cat-work-duration-seconds "work-duration"))

(defun pomo-cat--get-break-duration ()
  "Get validated short break duration in seconds."
  (pomo-cat--validate-positive-integer
   pomo-cat-break-duration-seconds "break-duration"))

(defun pomo-cat--get-long-break-duration ()
  "Get validated long break duration in seconds."
  (pomo-cat--validate-positive-integer
   pomo-cat-long-break-duration-seconds "long-break-duration"))

;;; Timer Management
;;
;; Safe timer operations to prevent race conditions.

(defun pomo-cat--cancel-timer ()
  "Safely cancel the current timer if it exists."
  (when-let* ((timer (pomo-cat--state-get :timer))
              ( (timerp timer) ))
    (cancel-timer timer)
    (pomo-cat--state-set :timer nil)))

(defun pomo-cat--schedule-timer (seconds callback)
  "Schedule CALLBACK to run after SECONDS, replacing any existing timer.
This ensures only one timer is active at a time."
  (pomo-cat--cancel-timer)
  (pomo-cat--state-set :timer (run-at-time seconds nil callback)))

(defun pomo-cat--clear-cat-display ()
  "Clear the current cat display, whether posframe, popon, or dedicated frame."
  (condition-case err
      (progn
        ;; Clean up posframe
        (when (and (featurep 'posframe) (posframe-workable-p))
          (posframe-delete "*pomo-cat*"))
        ;; Clean up popon
        (let ((popon (pomo-cat--state-get :popon-instance)))
          (when (and (featurep 'popon) popon)
            (popon-kill popon)
            (pomo-cat--state-set :popon-instance nil)))
        ;; Clean up dedicated frame
        (let ((frame (pomo-cat--state-get :dedicated-frame)))
          (when (and frame (frame-live-p frame))
            (delete-frame frame)
            (pomo-cat--state-set :dedicated-frame nil))))
    (error
     (message "pomo-cat: Error clearing display: %s"
              (error-message-string err)))))

;;; Display Utilities
;;
;; Common functions for display methods.

(defsubst pomo-cat--format-duration (seconds)
  "Format elapsed time as SECONDS."
  (format-seconds "%hh %mm %z"
                  (* 60 (round seconds 60))))

(require 'time-date)

(defsubst pomo-cat--format-end-time (end-time)
  "Display END-TIME either as remaining time or an end time."
  (let* ((recent   (time-add nil (* 60 60 2))))  ;; 2 hours
    (if (time-less-p recent end-time)
        (concat "until "
                (format-time-string "%I:%M %p" end-time))
      (concat "for another "
              (pomo-cat--format-duration
               (float-time (time-subtract end-time nil)))))))

(defun pomo-cat--theme-colors ()
  "Return a (foreground . background) cons cell based on current theme."
  (let* ((mode  (frame-parameter nil 'background-mode))
         (fg    (or (face-attribute 'default :foreground nil t)
                    (frame-parameter nil 'foreground-color)
                    (if (eq mode 'dark) "#ffffff" "#000000")))
         (bg    (or (face-attribute 'default :background nil t)
                    (frame-parameter nil 'background-color)
                    (if (eq mode 'dark) "#000000" "#ffffff"))))
    (cons fg bg)))

(defun pomo-cat--get-cat-text ()
  "Get the ASCII cat text, ensuring it is a string."
  (if (stringp pomo-cat-ascii-cat)
      pomo-cat-ascii-cat
    (format "%s" pomo-cat-ascii-cat)))

(defun pomo-cat--measure-ascii (text)
  "Return (width . height) of multiline ASCII TEXT."
  (let* ((lines (split-string text "\n"))
         (height (length lines))
         (width (apply #'max 0 (mapcar #'string-width lines))))
    (cons width height)))

(defun pomo-cat--show-posframe (content &optional width height)
  "Display CONTENT in a centered posframe with optional WIDTH and HEIGHT.
Uses theme colors for foreground/background."
  (when (and (featurep 'posframe) (display-graphic-p))
    (let* ((colors (pomo-cat--theme-colors))
           (fg (car colors))
           (bg (cdr colors)))
      (posframe-show
       "*pomo-cat*"
       :string content
       :position (point)
       :poshandler #'posframe-poshandler-frame-center
       :background-color bg
       :foreground-color fg
       :width width
       :height height))))

(defun pomo-cat--show-ascii-cat ()
  "Display ASCII art of the cat using `posframe` (GUI) or `popon` (terminal)."
  (let* ((cat-text (pomo-cat--get-cat-text))
         (size (pomo-cat--measure-ascii cat-text))
         (cols (car size))
         (lines (cdr size)))
    (cond
     ;; GUI: use posframe
     ((and (featurep 'posframe) (display-graphic-p))
      (pomo-cat--show-posframe cat-text cols lines))
     ;; Terminal: use popon
     ((featurep 'popon)
      (let* ((frame-width (frame-width))
             (frame-height (frame-height))
             (x (max 0 (/ (- frame-width cols) 2)))
             (y (max 0 (/ (- frame-height lines) 2))))
        (pomo-cat--state-set
         :popon-instance (popon-create cat-text `(,x . ,y)))))
     ;; Fallback: message area
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

(defun pomo-cat--image-available-p ()
  "Return non-nil if cat image is available for display."
  (and (display-graphic-p)
       (stringp pomo-cat-cat-image-path)
       (file-exists-p pomo-cat-cat-image-path)))

(defun pomo-cat--calculate-frame-geometry-for-image (img)
  "Calculate frame geometry for IMG display.
Returns plist with :left, :top, :width, :height."
  (let* ((img-width (car (image-size img t)))
         (img-height (cdr (image-size img t)))
         (char-width (frame-char-width))
         (char-height (frame-char-height))
         (cols (ceiling (/ (float img-width) char-width)))
         (lines (ceiling (/ (float img-height) char-height)))
         (workarea (frame-monitor-workarea))
         (work-left (nth 0 workarea))
         (work-top (nth 1 workarea))
         (work-width (nth 2 workarea))
         (work-height (nth 3 workarea))
         (border-pixels 16)
         (frame-width-pixels (+ img-width border-pixels))
         (frame-height-pixels (+ img-height border-pixels))
         (center-x
          (+ work-left (/ (- work-width frame-width-pixels) 2)))
         (center-y
          (+ work-top (/ (- work-height frame-height-pixels) 2))))
    (list
     :left (max 0 center-x)
     :top (max 0 center-y)
     :width cols
     :height lines)))

(defun pomo-cat--calculate-frame-geometry (text)
  "Calculate optimal frame geometry for TEXT display.
Returns plist with :left, :top, :width, :height."
  (let*
      ((size (pomo-cat--measure-ascii text))
       (cols (car size))
       (lines (cdr size))
       (workarea (frame-monitor-workarea))
       (work-left (nth 0 workarea))
       (work-top (nth 1 workarea))
       (work-width (nth 2 workarea))
       (work-height (nth 3 workarea))
       (char-width (frame-char-width))
       (char-height (frame-char-height))
       ;; Add margin for internal border (8px each side = 16px total)
       (border-pixels 16)
       ;; Calculate frame size in pixels for centering calculation
       (frame-width-pixels (+ (* cols char-width) border-pixels))
       (frame-height-pixels (+ (* lines char-height) border-pixels))
       ;; Calculate coordinates for center screen positioning
       (center-x
        (+ work-left (/ (- work-width frame-width-pixels) 2)))
       (center-y
        (+ work-top (/ (- work-height frame-height-pixels) 2))))
    (list
     :left (max 0 center-x)
     :top (max 0 center-y)
     :width cols
     :height lines)))

(defun pomo-cat--create-dedicated-frame (geometry)
  "Create dedicated frame with GEOMETRY.
GEOMETRY is a plist with :left, :top, :width, :height."
  (let*
      ((colors (pomo-cat--theme-colors))
       (target-width (plist-get geometry :width))
       (target-height (plist-get geometry :height))
       (base-params
        `((name . "🐱 Pomo Cat Break")
          (title . "Take a Break!")
          ;; Size in characters
          (width . ,target-width)
          (height . ,target-height)
          (left . ,(plist-get geometry :left))
          (top . ,(plist-get geometry :top))
          (user-position . t)
          (background-color . ,(cdr colors))
          (foreground-color . ,(car colors))
          ;; Minimal UI settings
          (tool-bar-lines . 0)
          (menu-bar-lines . 0)
          (tab-bar-lines . 0)
          (vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)
          (left-fringe . 0)
          (right-fringe . 0)
          (internal-border-width . 8)
          ;; Focus control
          (no-focus-on-map . t)
          ;; Other settings
          (unsplittable . t)
          (minibuffer . nil)))
       ;; Temporarily disable default-frame-alist to prevent override
       (frame
        (let ((default-frame-alist nil))
          (make-frame base-params))))
    frame))

(defun pomo-cat--show-dedicated-frame ()
  "Display cat in a dedicated Emacs frame using only built-in functions.
Displays image if available, otherwise falls back to ASCII art."
  (let* ((use-image (pomo-cat--image-available-p))
         (img
          (when use-image
            (create-image pomo-cat-cat-image-path)))
         (geometry
          (if use-image
              (pomo-cat--calculate-frame-geometry-for-image img)
            (pomo-cat--calculate-frame-geometry
             (pomo-cat--get-cat-text))))
         (frame (pomo-cat--create-dedicated-frame geometry)))
    (pomo-cat--state-set :dedicated-frame frame)
    (with-selected-frame frame
      (switch-to-buffer "*pomo-cat-break*")
      (read-only-mode -1)
      (erase-buffer)
      (if use-image
          (insert-image img)
        (insert (pomo-cat--get-cat-text)))
      (goto-char (point-min))
      (setq-local mode-line-format nil)
      (read-only-mode 1))))

(defun pomo-cat--manage-frame-focus (frame)
  "Manage focus behavior for FRAME according to `pomo-cat-get-focus'."
  (when (and frame (frame-live-p frame))
    (with-selected-frame frame
      ;; Platform-specific focus control
      (when (fboundp 'x-focus-frame)
        (x-focus-frame frame))
      (when (fboundp 'raise-frame)
        (raise-frame frame)))
    ;; Return focus based on settings
    (unless pomo-cat-get-focus
      (other-frame 0))))

(defun pomo-cat--show-cat ()
  "Show a cat based on configuration and environment.
- If `pomo-cat-use-dedicated-frame' is non-nil and GUI: dedicated-frame
- If image is configured and GUI: posframe with image
- Otherwise: popon with ASCII art"
  (condition-case err
      (cond
       ;; Dedicated frame (GUI only)
       ((and pomo-cat-use-dedicated-frame (display-graphic-p))
        (pomo-cat--show-dedicated-frame)
        (when pomo-cat-get-focus
          (pomo-cat--manage-frame-focus
           (pomo-cat--state-get :dedicated-frame))))

       ;; Posframe with image (GUI only, when image is configured)
       ((pomo-cat--image-available-p)
        (pomo-cat--show-image))

       ;; Default: ASCII cat (popon or fallback)
       (t
        (pomo-cat--show-ascii-cat)))
    (error
     (message "pomo-cat: Error showing cat: %s"
              (error-message-string err)))))

(defun pomo-cat--start-break ()
  "Begin a short or long break and show cat display."
  (let* ((cycle-count (pomo-cat--state-get :cycle-count))
         (break-type
          (if (zerop
               (% cycle-count pomo-cat-cycles-before-long-break))
              'long
            'short))
         (duration
          (if (eq break-type 'long)
              (pomo-cat--get-long-break-duration)
            (pomo-cat--get-break-duration))))
    (pomo-cat--state-set :break-type break-type)
    (pomo-cat--state-set :in-break t)
    (message "Pomodoro Break started! (a %s break for %s)"
             break-type
             (pomo-cat--format-duration duration))
    (pomo-cat--show-cat)
    (when pomo-cat-get-focus
      (other-frame 0))
    (pomo-cat--schedule-timer duration #'pomo-cat--start-work)))

(defun pomo-cat--start-work ()
  "Begin a new Pomodoro work session."
  (pomo-cat--clear-cat-display)
  (let ((new-count (1+ (or (pomo-cat--state-get :cycle-count) 0)))
        (duration (pomo-cat--get-work-duration)))
    (pomo-cat--state-set :cycle-count new-count)
    (pomo-cat--state-set :in-break nil)
    (message "Pomodoro Work #%d has started! (%s)"
             new-count
             (pomo-cat--format-duration duration))
    (pomo-cat--schedule-timer duration #'pomo-cat--start-break)))

;;;###autoload
(defun pomo-cat-start ()
  "Start the Pomodoro timer."
  (interactive)
  ;; Stop any existing timer and clear display
  (pomo-cat--clear-cat-display)
  (pomo-cat--reset-state)
  (pomo-cat--start-work))

;;;###autoload
(defun pomo-cat-stop ()
  "Stop the Pomodoro timer and clear cat display."
  (interactive)
  (pomo-cat--clear-cat-display)
  (pomo-cat--reset-state)
  (message "Pomodoro stopped."))

;;;###autoload
(defun pomo-cat-delay-break (&optional seconds)
  "Delay the current break for SECONDS.
If SECONDS is nil, uses `pomo-cat-delay-break-seconds'.
If SECONDS is negative, it is treated as 0."
  (interactive "P")
  (if (not (pomo-cat--state-get :in-break))
      (message "pomo-cat: Not currently in a break.")
    (let ((delay (max 0 (or seconds pomo-cat-delay-break-seconds))))
      (pomo-cat--clear-cat-display)
      (pomo-cat--state-set :in-break nil) ; Mark as not in break during delay
      (pomo-cat--schedule-timer delay #'pomo-cat--start-break)
      (message "Pomodoro Break delayed %s."
               (pomo-cat--format-duration delay)))))

;;;###autoload
(defun pomo-cat-stop-break ()
  "Simply stop the current break period as if it has finished."
  (interactive)
  (if (not (pomo-cat--state-get :in-break))
      (message "pomo-cat: Not currently in a break.")
    (pomo-cat--cancel-timer)
    (pomo-cat--start-work)))

;;;###autoload
(defun pomo-cat-status ()
  "Display current pomo-cat status."
  (interactive)
  (let ((timer (pomo-cat--state-get :timer))
        (cycle (or (pomo-cat--state-get :cycle-count) 0))
        (in-break (pomo-cat--state-get :in-break))
        (break-type (pomo-cat--state-get :break-type)))
    (if (not timer)
        (message "pomo-cat: Not running")
      (message "Pomodoro Cycle #%d, %s %s"
               cycle
               (if in-break
                   (format "in a %s break" break-type)
                 "working")
               (pomo-cat--format-end-time (timer--time timer))))))

;;; Initialization

;; Initialize state on load
(pomo-cat--reset-state)

(provide 'pomo-cat)

;;; pomo-cat.el ends here
