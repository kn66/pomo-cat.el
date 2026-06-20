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
(declare-function posframe-poshandler-frame-top-left-corner "posframe")
(declare-function posframe-poshandler-frame-top-right-corner "posframe")
(declare-function posframe-poshandler-frame-bottom-left-corner "posframe")
(declare-function posframe-poshandler-frame-bottom-right-corner "posframe")
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
When set to symbol `topmost', the dedicated frame is also requested
to stay above normal windows (platform/window-manager dependent).
When nil, uses posframe (if image is configured) or popon (ASCII art)."
  :type '(choice
          (const :tag "Disabled" nil)
          (const :tag "Dedicated frame" t)
          (const :tag "Dedicated frame (always on top)" topmost)))

(defcustom pomo-cat-dedicated-frame-position 'center
  "Position of the dedicated break frame on screen.
This setting is used only when `pomo-cat-use-dedicated-frame' is non-nil."
  :type '(choice
          (const :tag "Center" center)
          (const :tag "Top left" top-left)
          (const :tag "Top right" top-right)
          (const :tag "Bottom left" bottom-left)
          (const :tag "Bottom right" bottom-right)))

(defcustom pomo-cat-dedicated-frame-margin-pixels 24
  "Margin in pixels from screen edges for dedicated frame corner positions.
This setting is ignored when `pomo-cat-dedicated-frame-position' is `center'."
  :type 'integer)

(defcustom pomo-cat-overlay-position 'center
  "Position of non-dedicated break display (`posframe' / `popon').
This setting is used when `pomo-cat-use-dedicated-frame' is nil."
  :type '(choice
          (const :tag "Center" center)
          (const :tag "Top left" top-left)
          (const :tag "Top right" top-right)
          (const :tag "Bottom left" bottom-left)
          (const :tag "Bottom right" bottom-right)))

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
  :display-ticker - Repeating timer object for break display updates
  :cycle-count - Number of completed work sessions
  :break-type - Current break type (\\='short or \\='long)
  :in-break - Whether currently in a break
  :phase-end-time - End time of current phase as a time value
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
  (let ((ticker (pomo-cat--state-get :display-ticker)))
    (when ticker
      (cancel-timer ticker)))
  (setq pomo-cat--state
        '(:timer
          nil
          :display-ticker nil
          :cycle-count 0
          :break-type short
          :in-break nil
          :phase-end-time nil
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

(defun pomo-cat--get-cycles-before-long-break ()
  "Get validated cycle count before long break."
  (let ((value pomo-cat-cycles-before-long-break))
    (cond
     ((and (integerp value) (> value 0))
      value)
     ((and (numberp value) (> value 0))
      (let ((rounded (round value)))
        (message "pomo-cat: cycles-before-long-break should be integer, using %d"
                 rounded)
        rounded))
     (t
      (message "pomo-cat: Invalid cycles-before-long-break (%s), using default 4"
               value)
      4))))

;;; Timer Management
;;
;; Safe timer operations to prevent race conditions.

(defun pomo-cat--cancel-timer ()
  "Safely cancel the current timer if it exists."
  (let ((timer (pomo-cat--state-get :timer)))
    (when (and timer (timerp timer))
      (cancel-timer timer)
      (pomo-cat--state-set :timer nil))))

(defun pomo-cat--cancel-display-ticker ()
  "Safely cancel the display update ticker if it exists."
  (let ((ticker (pomo-cat--state-get :display-ticker)))
    (when (and ticker (timerp ticker))
      (cancel-timer ticker)
      (pomo-cat--state-set :display-ticker nil))))

(defun pomo-cat--schedule-timer (seconds callback)
  "Schedule CALLBACK to run after SECONDS, replacing any existing timer.
This ensures only one timer is active at a time."
  (pomo-cat--cancel-timer)
  (pomo-cat--state-set :timer (run-at-time seconds nil callback)))

(defun pomo-cat--clear-cat-display ()
  "Clear the current cat display, whether posframe, popon, or dedicated frame."
  (condition-case err
      (progn
        (pomo-cat--cancel-display-ticker)
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

(defun pomo-cat--theme-colors ()
  "Return a (foreground . background) cons cell based on current theme."
  (let ((fg
         (or (face-attribute 'default :foreground nil t) "#ffffff"))
        (bg
         (or (face-attribute 'default :background nil t) "#000000")))
    (cons fg bg)))

(defun pomo-cat--get-cat-text ()
  "Get the ASCII cat text, ensuring it is a string."
  (let ((text (if (stringp pomo-cat-ascii-cat)
                  pomo-cat-ascii-cat
                (format "%s" pomo-cat-ascii-cat))))
    ;; Multiline literals often include leading/trailing blank lines.
    ;; Trim only outer newlines so the visible content fits predictably.
    (replace-regexp-in-string
     "\n+\\'" ""
     (replace-regexp-in-string "\\`\n+" "" text))))

(defun pomo-cat--remaining-break-seconds ()
  "Return remaining break time in seconds, or nil if unavailable."
  (let ((end-time (pomo-cat--state-get :phase-end-time)))
    (when end-time
      (max 0
           (ceiling
            (float-time
             (time-subtract end-time (current-time))))))))

(defun pomo-cat--format-mmss (seconds)
  "Format SECONDS as MM:SS."
  (format "%02d:%02d" (/ seconds 60) (% seconds 60)))

(defun pomo-cat--break-remaining-text ()
  "Return break remaining-time string, or nil outside breaks."
  (when (pomo-cat--state-get :in-break)
    (let ((remaining (pomo-cat--remaining-break-seconds)))
      (when (integerp remaining)
        (format "Remaining: %s" (pomo-cat--format-mmss remaining))))))

(defun pomo-cat--ascii-cat-display-text ()
  "Return ASCII cat text with optional break countdown line."
  (let ((cat-text (pomo-cat--get-cat-text))
        (remaining-text (pomo-cat--break-remaining-text)))
    (if remaining-text
        (concat remaining-text "\n" cat-text)
      cat-text)))

(defun pomo-cat--measure-ascii (text)
  "Return (width . height) of multiline ASCII TEXT."
  (let* ((lines (split-string text "\n"))
         (height (length lines))
         (width (apply #'max 0 (mapcar #'string-width lines))))
    (cons width height)))

(defun pomo-cat--show-posframe (content &optional width height)
  "Display CONTENT in a posframe with optional WIDTH and HEIGHT.
Uses theme colors and `pomo-cat-overlay-position'."
  (when (and (featurep 'posframe)
             (display-graphic-p)
             (posframe-workable-p))
    (let* ((colors (pomo-cat--theme-colors))
           (fg (car colors))
           (bg (cdr colors)))
      (posframe-show
       "*pomo-cat*"
       :string content
       :position (point)
       :poshandler (pomo-cat--posframe-poshandler)
       :background-color bg
       :foreground-color fg
       :width width
       :height height)
      t)))

(defun pomo-cat--posframe-poshandler ()
  "Return posframe poshandler function for `pomo-cat-overlay-position'."
  (pcase pomo-cat-overlay-position
    ('top-left #'posframe-poshandler-frame-top-left-corner)
    ('top-right #'posframe-poshandler-frame-top-right-corner)
    ('bottom-left #'posframe-poshandler-frame-bottom-left-corner)
    ('bottom-right #'posframe-poshandler-frame-bottom-right-corner)
    (_ #'posframe-poshandler-frame-center)))

(defun pomo-cat--popon-position (cols lines)
  "Return `(x . y)' position for a popon of COLS x LINES."
  (let* ((frame-cols (frame-width))
         (frame-lines (frame-height))
         (center-x (max 0 (/ (- frame-cols cols) 2)))
         (center-y (max 0 (/ (- frame-lines lines) 2)))
         (x center-x)
         (y center-y))
    (pcase pomo-cat-overlay-position
      ('top-left
       (setq x 0 y 0))
      ('top-right
       (setq x (max 0 (- frame-cols cols))
             y 0))
      ('bottom-left
       (setq x 0
             y (max 0 (- frame-lines lines))))
      ('bottom-right
       (setq x (max 0 (- frame-cols cols))
             y (max 0 (- frame-lines lines)))))
    (cons x y)))

(defun pomo-cat--show-ascii-cat ()
  "Display ASCII art of the cat using `posframe` (GUI) or `popon` (terminal)."
  (let* ((cat-text (pomo-cat--ascii-cat-display-text))
         (size (pomo-cat--measure-ascii cat-text))
         (cols (car size))
         (lines (cdr size)))
    (cond
     ;; GUI: use posframe
     ((and (featurep 'posframe)
           (display-graphic-p)
           (posframe-workable-p))
      (pomo-cat--show-posframe cat-text cols lines))
     ;; Terminal: use popon
     ((featurep 'popon)
      (let ((old-popo (pomo-cat--state-get :popon-instance)))
        (when old-popo
          (ignore-errors (popon-kill old-popo))
          (pomo-cat--state-set :popon-instance nil)))
      (pomo-cat--state-set
       :popon-instance (popon-create cat-text (pomo-cat--popon-position cols lines))))
     ;; Fallback: message area
     (t
      (message "\n%s" cat-text)))))

(defun pomo-cat--show-image ()
  "Display the configured cat image using posframe, if available."
  (when (and (featurep 'posframe)
             (display-graphic-p)
             (posframe-workable-p)
             (stringp pomo-cat-cat-image-path)
             (file-exists-p pomo-cat-cat-image-path))
    (condition-case err
        (let* ((remaining-text (pomo-cat--break-remaining-text))
               (img (create-image pomo-cat-cat-image-path))
               (width (car (image-size img t)))
               (height (cdr (image-size img t)))
               (char-width (frame-char-width))
               (char-height (frame-char-height))
               (img-cols (ceiling (/ (float width) char-width)))
               (img-lines (ceiling (/ (float height) char-height)))
               (text-cols (if remaining-text
                              (string-width remaining-text)
                            0))
               (cols (max img-cols text-cols))
               ;; Reserve an extra line for countdown and one safety line
               ;; because image row height rounding can clip the last line.
               (lines (+ img-lines (if remaining-text 2 0))))
          (posframe-show
           "*pomo-cat*"
           :string ""
           :poshandler (pomo-cat--posframe-poshandler)
           :width cols
           :height lines)
          (with-current-buffer "*pomo-cat*"
            (erase-buffer)
            (insert-image img)
            (when remaining-text
              (insert "\n" remaining-text)))
          t)
      (error
       (message "pomo-cat: Error showing image: %s"
                (error-message-string err))
       nil))))

(defun pomo-cat--image-available-p ()
  "Return non-nil if cat image is available for display."
  (and (display-graphic-p)
       (stringp pomo-cat-cat-image-path)
       (file-exists-p pomo-cat-cat-image-path)))

(defun pomo-cat--calculate-frame-geometry-for-image (img &optional remaining-text)
  "Calculate frame geometry for IMG display.
Returns plist with :left, :top, :width, :height."
  (let* ((img-width (car (image-size img t)))
         (img-height (cdr (image-size img t)))
         (char-width (frame-char-width))
         (char-height (frame-char-height))
         (img-cols (ceiling (/ (float img-width) char-width)))
         (img-lines (ceiling (/ (float img-height) char-height)))
         (text-cols (if remaining-text (string-width remaining-text) 0))
         ;; Keep one extra safety line to avoid clipping countdown text.
         (extra-lines (if remaining-text 2 0))
         (cols (max img-cols text-cols))
         (lines (+ img-lines extra-lines))
         (border-pixels 16)
         (frame-width-pixels
          (+ (max img-width (* text-cols char-width)) border-pixels))
         (frame-height-pixels
          (+ img-height (* extra-lines char-height) border-pixels))
         (position (pomo-cat--dedicated-frame-screen-position
                    frame-width-pixels frame-height-pixels)))
    (list
     :left (plist-get position :left)
     :top (plist-get position :top)
     :width cols
     :height lines)))

(defun pomo-cat--update-dedicated-frame-content (frame use-image img remaining-text)
  "Update FRAME buffer contents with cat display.
USE-IMAGE selects between IMG and ASCII.
REMAINING-TEXT is appended when non-nil."
  (with-selected-frame frame
    (switch-to-buffer "*pomo-cat-break*")
    (read-only-mode -1)
    (erase-buffer)
    (if use-image
        (progn
          (insert-image img)
          (when remaining-text
            (insert "\n" remaining-text)))
      (insert (pomo-cat--ascii-cat-display-text)))
    (goto-char (point-min))
    (set-window-point (selected-window) (point-min))
    (set-window-start (selected-window) (point-min))
    (setq-local cursor-type nil)
    (setq-local mode-line-format nil)
    (read-only-mode 1)))

(defun pomo-cat--apply-frame-geometry (frame geometry)
  "Apply GEOMETRY plist to FRAME."
  (when (and frame (frame-live-p frame))
    (set-frame-position frame
                        (plist-get geometry :left)
                        (plist-get geometry :top))
    (set-frame-size frame
                    (plist-get geometry :width)
                    (plist-get geometry :height))))

(defun pomo-cat--apply-dedicated-frame-mode (frame)
  "Apply mode-specific parameters to dedicated FRAME."
  (when (and frame (frame-live-p frame))
    ;; `z-group' support depends on the GUI backend and window manager.
    ;; Ignore errors so dedicated-frame mode still works everywhere.
    (condition-case nil
        (set-frame-parameter
         frame 'z-group
         (when (eq pomo-cat-use-dedicated-frame 'topmost) 'above))
      (error nil))))

(defun pomo-cat--dedicated-frame-screen-position (frame-width-pixels frame-height-pixels)
  "Calculate dedicated frame screen position for given pixel dimensions.
Returns a plist with `:left' and `:top'."
  (let* ((workarea (frame-monitor-workarea))
         (work-left (nth 0 workarea))
         (work-top (nth 1 workarea))
         (work-width (nth 2 workarea))
         (work-height (nth 3 workarea))
         (margin (max 0 pomo-cat-dedicated-frame-margin-pixels))
         (max-left (+ work-left (max 0 (- work-width frame-width-pixels))))
         (max-top (+ work-top (max 0 (- work-height frame-height-pixels))))
         (center-left (+ work-left (/ (- work-width frame-width-pixels) 2)))
         (center-top (+ work-top (/ (- work-height frame-height-pixels) 2)))
         (left center-left)
         (top center-top))
    (pcase pomo-cat-dedicated-frame-position
      ('top-left
       (setq left (+ work-left margin)
             top (+ work-top margin)))
      ('top-right
       (setq left (- (+ work-left work-width) frame-width-pixels margin)
             top (+ work-top margin)))
      ('bottom-left
       (setq left (+ work-left margin)
             top (- (+ work-top work-height) frame-height-pixels margin)))
      ('bottom-right
       (setq left (- (+ work-left work-width) frame-width-pixels margin)
             top (- (+ work-top work-height) frame-height-pixels margin))))
    (list :left (min max-left (max work-left left))
          :top (min max-top (max work-top top)))))

(defun pomo-cat--calculate-frame-geometry (text)
  "Calculate optimal frame geometry for TEXT display.
Returns plist with :left, :top, :width, :height."
  (let*
      ((size (pomo-cat--measure-ascii text))
       ;; Keep a small safety margin.  Some GUI builds/platforms clip the last
       ;; row/column when the frame is resized to the exact text dimensions.
       (cols (+ 2 (car size)))
       (lines (+ 2 (cdr size)))
       (char-width (frame-char-width))
       (char-height (frame-char-height))
       ;; Add margin for internal border (8px each side = 16px total)
       (border-pixels 16)
       ;; Calculate frame size in pixels for centering calculation
       (frame-width-pixels (+ (* cols char-width) border-pixels))
       (frame-height-pixels (+ (* lines char-height) border-pixels))
       (position (pomo-cat--dedicated-frame-screen-position
                  frame-width-pixels frame-height-pixels)))
    (list
     :left (plist-get position :left)
     :top (plist-get position :top)
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
          (minibuffer . nil)
          ,@(when (eq pomo-cat-use-dedicated-frame 'topmost)
              '((z-group . above)))))
       ;; Temporarily disable default-frame-alist to prevent override
       (frame
        (let ((default-frame-alist nil))
          (make-frame base-params))))
    frame))

(defun pomo-cat--show-dedicated-frame ()
  "Display cat in a dedicated Emacs frame using only built-in functions.
Displays image if available, otherwise falls back to ASCII art."
  (let* ((use-image (pomo-cat--image-available-p))
         (remaining-text (pomo-cat--break-remaining-text))
         (img nil))
    (when use-image
      (condition-case err
          (setq img (create-image pomo-cat-cat-image-path))
        (error
         (setq use-image nil)
         (message "pomo-cat: Error preparing dedicated-frame image: %s"
                  (error-message-string err)))))
    (let* (
         (geometry
          (if use-image
              (pomo-cat--calculate-frame-geometry-for-image img remaining-text)
            (pomo-cat--calculate-frame-geometry
             (pomo-cat--ascii-cat-display-text))))
         (frame (pomo-cat--state-get :dedicated-frame)))
      (unless (and frame (frame-live-p frame))
        (setq frame (pomo-cat--create-dedicated-frame geometry))
        (pomo-cat--state-set :dedicated-frame frame))
      (pomo-cat--apply-dedicated-frame-mode frame)
      (pomo-cat--apply-frame-geometry frame geometry)
      (pomo-cat--update-dedicated-frame-content frame use-image img remaining-text))))

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

(defun pomo-cat--show-cat (&optional skip-focus-management)
  "Show a cat based on configuration and environment.
- If `pomo-cat-use-dedicated-frame' is non-nil and GUI: dedicated-frame
- If image is configured and GUI: posframe with image
- Otherwise: popon with ASCII art"
  (condition-case err
      (cond
       ;; Dedicated frame (GUI only)
       ((and pomo-cat-use-dedicated-frame (display-graphic-p))
        (pomo-cat--show-dedicated-frame)
        (when (and pomo-cat-get-focus (not skip-focus-management))
          (pomo-cat--manage-frame-focus
           (pomo-cat--state-get :dedicated-frame))))

       ;; Posframe with image (GUI only, when image is configured)
       ((pomo-cat--image-available-p)
        (or (pomo-cat--show-image)
            (pomo-cat--show-ascii-cat)))

       ;; Default: ASCII cat (popon or fallback)
       (t
        (pomo-cat--show-ascii-cat)))
    (error
     (message "pomo-cat: Error showing cat: %s"
              (error-message-string err)))))

(defun pomo-cat--display-ticker-supported-p ()
  "Return non-nil when break display can be refreshed in-place."
  (or (and pomo-cat-use-dedicated-frame (display-graphic-p))
      (and (featurep 'posframe)
           (display-graphic-p)
           (posframe-workable-p))
      (featurep 'popon)))

(defun pomo-cat--refresh-break-display ()
  "Refresh the visible break display countdown."
  (if (not (pomo-cat--state-get :in-break))
      (pomo-cat--cancel-display-ticker)
    (pomo-cat--show-cat t)))

(defun pomo-cat--start-display-ticker ()
  "Start the 1-second break display refresh ticker."
  (pomo-cat--cancel-display-ticker)
  (when (and (pomo-cat--state-get :in-break)
             (pomo-cat--display-ticker-supported-p))
    (pomo-cat--state-set
     :display-ticker
     (run-at-time 1 1 #'pomo-cat--refresh-break-display))))

(defun pomo-cat--start-break ()
  "Begin a short or long break and show cat display."
  (let* ((cycle-count (pomo-cat--state-get :cycle-count))
         (cycles-before-long-break
          (pomo-cat--get-cycles-before-long-break))
         (break-type
          (if (zerop
               (% cycle-count cycles-before-long-break))
              'long
            'short))
         (duration
          (if (eq break-type 'long)
              (pomo-cat--get-long-break-duration)
            (pomo-cat--get-break-duration))))
    (pomo-cat--state-set :break-type break-type)
    (pomo-cat--state-set :in-break t)
    (pomo-cat--state-set :phase-end-time
                         (time-add (current-time)
                                   (seconds-to-time duration)))
    (message "Break started! (%s break)" (symbol-name break-type))
    (pomo-cat--show-cat)
    (pomo-cat--start-display-ticker)
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
    (pomo-cat--state-set :phase-end-time nil)
    (message "Pomodoro work #%d started!" new-count)
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
  (interactive
   (list
    (when current-prefix-arg
      (prefix-numeric-value current-prefix-arg))))
  (if (not (pomo-cat--state-get :in-break))
      (message "pomo-cat: Not currently in a break.")
    (let* ((raw-delay (or seconds pomo-cat-delay-break-seconds))
           (delay
            (cond
             ((integerp raw-delay)
              (max 0 raw-delay))
             ((numberp raw-delay)
              (max 0 (round raw-delay)))
             (t
              (message "pomo-cat: Invalid delay (%s), using default %d"
                       raw-delay
                       pomo-cat-delay-break-seconds)
              (max 0 pomo-cat-delay-break-seconds)))))
      (pomo-cat--clear-cat-display)
      (pomo-cat--state-set :in-break nil) ; Mark as not in break during delay
      (pomo-cat--state-set :phase-end-time nil)
      (pomo-cat--schedule-timer delay #'pomo-cat--start-break)
      (message "Break delayed %ds." delay))))

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
      (message "pomo-cat: Cycle #%d, %s"
               cycle
               (if in-break
                   (format "in %s break" (symbol-name break-type))
                 "working")))))

(provide 'pomo-cat)

;;; pomo-cat.el ends here
