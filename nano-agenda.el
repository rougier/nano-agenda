;;; nano-agenda.el --- N Λ N O agenda -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-agenda
;; Version: 0.2.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, org-mode, org-agenda

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; N Λ N O agenda is a minimal view of your org agenda files. It
;; displays a calendar view of current month (or the month
;; corresponding to the current selected date) alongside a view of
;; your agenda displaying timestamped entries. The agenda can be
;; navigated using arrows keys and killed using "q", "return" or
;; "escape".
;;
;; Usage example:
;;
;; M-x: nano-agenda
;;
;;; NEWS:
;;
;; Version 0.2.1
;; - Removed ts requirements
;;
;; Version 0.2
;; - Removed ts (MELPA) dependency
;;
;; Version 0.1
;; - Submission to ELPA
;;
;;; Code 
(require 'org)
(require 'cl-lib)
(require 'org-agenda)
(require 'org-duration)
(require 'calendar)
(require 'holidays)


(defgroup nano-agenda nil
  "N Λ N O agenda"
  :group 'nano)

(defgroup nano-agenda-faces nil
  "N Λ N O agenda faces"
  :group 'nano-agenda)

(defvar nano-agenda--current-selection (current-time)
  "Current selected date")

(defvar nano-agenda--busy-levels (list)
  "Cached list of (date busy-level) for internal use")

(defcustom nano-agenda-today-symbol  "•"
  "Symbol to show current day"
  :group 'nano-agenda)

(defcustom nano-agenda-sort-function #'nano-agenda-default-sort-function
  "Function to sort a day's entries.
This function takes an entries list and returns the list in the desired order."
  :group 'nano-agenda)

(defcustom nano-agenda-select-entry-predicate #'nano-agenda-select-entry
  "Predicate to decide if entry will be shown in the nano-agenda buffer.
This function takes an entry and the selected date. Returns a value if the entry
should be shown, otherwise, returns nil."
  :group 'nano-agenda)

(defcustom nano-agenda-busy-backgrounds  (list "#FFF9DB" "#FFF3BF" "#FFEC99" "#FFE066" "#FFD43B"
                                               "#FCC419" "#FAB005" "#F59F00" "#F08C00" "#E67700")
  "Background colors to be used to highlight a day in calendar
  view according to busy level."
  :type '(repeat color)
  :group 'nano-agenda-faces)

(defcustom nano-agenda-busy-foregrounds (list "#000000" "#000000" "#000000" "#000000" "#000000"
                                              "#000000" "#000000" "#000000" "#000000" "#FFFFFF")
  "Foreground colors to be used to highlight a day in calendar
  view according to busy level."
  :type '(repeat color)
  :group 'nano-agenda-faces)

(defface nano-agenda-default
  '((t :inherit default))
  "Default face (for casual day)"
  :group 'nano-agenda-faces)

(defface nano-agenda-selected
  '((t :inherit default :inverse-video t))
  "Face for the selected day"
  :group 'nano-agenda-faces)

(defface nano-agenda-weekend
  '((t :inherit font-lock-comment-face))
  "Weekend face"
  :group 'nano-agenda-faces)

(defface nano-agenda-holidays
  '((t :inherit font-lock-comment-face))
  "Holidays face"
  :group 'nano-agenda-faces)

(defface nano-agenda-outday
  '((t :inherit font-lock-comment-face))
  "Out day face, that is, day outside curent month."
  :group 'nano-agenda-faces)

(defface nano-agenda-day-name
  '((t :inherit font-lock-comment-face))
  "Day name face (on second line)"
  :group 'nano-agenda-faces)

(defface nano-agenda-month-name
  '((t :inherit bold))
  "Month name face (on first line)"
  :group 'nano-agenda-faces)

(defface nano-agenda-mouse
  '((t :inherit 'highlight ))
  "Mouse highlight face"
  :group 'nano-agenda-faces)

(defface nano-agenda-button
  '((t :inherit font-lock-comment-face))
  "Header button (left and right)"
  :group 'nano-agenda-faces)

(defun nano-agenda-date (year month day)
  "Return a date correspondng to DAY/MONTH/YEAR."
  (encode-time 0 0 0 day month year))

(defun nano-agenda-date-equal (date1 date2)
  "Check if DATE1 is equal to DATE2."
  (and (eq (nano-agenda-date-day date1)
           (nano-agenda-date-day date2))
       (eq (nano-agenda-date-month date1)
           (nano-agenda-date-month date2))
       (eq (nano-agenda-date-year date1)
           (nano-agenda-date-year date2))))

(defun nano-agenda-date-inc (date &optional days months years)
  "Return DATE + DAYS day & MONTH months & YEARS years"
  (let ((days (or days 0))
        (months (or months 0))
        (years (or years 0))
        (day (nano-agenda-date-day date))
        (month (nano-agenda-date-month date))
        (year (nano-agenda-date-year date)))
    (encode-time 0 0 0 (+ day days) (+ month months) (+ year years))))

(defun nano-agenda-date-dec (date &optional days months years)
  "Return DATE - DAYS day & MONTH months & YEARS years"
  (let ((days (or days 0))
        (months (or months 0))
        (years (or years 0)))
    (nano-agenda-date-inc date (- days) (- months) (- years))))


(defun nano-agenda-date-day (date)
  "Return DATE day of month (1-31)."
  (nth 3 (decode-time date)))
                        
(defun nano-agenda-date-month (date)
  "Return DATE month number (1-12)."
  (nth 4 (decode-time date)))

(defun nano-agenda-date-year (date)
  "Return DATE year."
  (nth 5 (decode-time date)))

(defun nano-agenda-date-doy (date)
  "Return DATE day of year (1-366)."
  (string-to-number (format-time-string "%j" date)))

(defun nano-agenda-date-dow (date)
  "Return DATE day of week (0-6)."
  (nth 6 (decode-time date)))

(defun nano-agenda-date-day-name (date)
  "Return DATE full day name."
  (format-time-string "%A" date))

(defun nano-agenda-date-month-name (date)
  "Return DATE full month name."
  (format-time-string "%B" date))

(defun nano-agenda-date-is-today (date)
  "Check if DATE is today."
  (nano-agenda-date-equal (current-time) date))

(defun nano-agenda-date-today ()
  "Return today date."
  (current-time))

(defun nano-agenda-date-tomorrow ()
  "Return tomorrow date."
  (nano-agenda-date-inc (nano-agenda-date-today) 1 0 0))
  
(defun nano-agenda-yesterday ()
  "Return yesterday date."
  (nano-agenda-date-dec (nano-agenda-date-today) 1 0 0))

(defun nano-agenda-forward-day ()
  (interactive)
  (setq nano-agenda--current-selection
        (nano-agenda-date-inc nano-agenda--current-selection 1))
  (nano-agenda-update))

(defun nano-agenda-backward-day ()
  (interactive)
  (setq nano-agenda--current-selection
        (nano-agenda-date-dec nano-agenda--current-selection 1))
  (nano-agenda-update))

(defun nano-agenda-forward-week ()
  (interactive)
  (setq nano-agenda--current-selection
        (nano-agenda-date-inc nano-agenda--current-selection 7))
  (nano-agenda-update))

(defun nano-agenda-backward-week ()
  (interactive)
  (setq nano-agenda--current-selection
        (nano-agenda-date-dec nano-agenda--current-selection 7))
  (nano-agenda-update))

(defun nano-agenda-forward-month ()
  (interactive)
  (setq nano-agenda--current-selection
        (nano-agenda-date-inc nano-agenda--current-selection 0 1))
  (nano-agenda-update))

(defun nano-agenda-backward-month ()
  (interactive)
  (setq nano-agenda--current-selection
        (nano-agenda-date-dec nano-agenda--current-selection 0 1))
  (nano-agenda-update))
  
(defun nano-agenda-forward-year ()
  (interactive)
  (setq nano-agenda--current-selection
        (nano-agenda-date-inc nano-agenda--current-selection 0 0 1))
  (nano-agenda-update))
    
(defun nano-agenda-backward-year ()
  (interactive)
  (setq nano-agenda--current-selection
        (nano-agenda-date-dec nano-agenda--current-selection 0 0 1))
  (nano-agenda-update))

(defun nano-agenda-goto-today ()
  (interactive)
  (setq nano-agenda--current-selection (nano-agenda-date-today))
  (nano-agenda-update))

(defun nano-agenda-goto (&optional date)
  (interactive)
  (setq nano-agenda--current-selection (or date (nano-agenda-date-today)))
  (nano-agenda-update))

(define-minor-mode nano-agenda-mode
  "Minor mode for nano-agenda."
  :init nil
  :keymap `((,(kbd "<left>")    . nano-agenda-backward-day)
            (,(kbd "<right>")   . nano-agenda-forward-day)
            (,(kbd "<up>")      . nano-agenda-backward-week)
            (,(kbd "<down>")    . nano-agenda-forward-week)
            (,(kbd "<S-left>")  . nano-agenda-backward-month)
            (,(kbd "<S-right>") . nano-agenda-forward-month)
            (,(kbd "<S-up>")    . nano-agenda-backward-year)
            (,(kbd "<S-down>")  . nano-agenda-forward-year)
            (,(kbd "r")         . nano-agenda-refresh)
            (,(kbd ".")         . nano-agenda-goto-today)
            (,(kbd "t")         . nano-agenda-goto-today)
            (,(kbd "C-g")       . nano-agenda-kill)
            (,(kbd "q")         . nano-agenda-kill)
            (,(kbd "<return>")  . nano-agenda-kill)
            (,(kbd "<escape>")  . nano-agenda-kill))

  (when nano-agenda-mode
    (setq buffer-read-only t)
    (setq cursor-type nil)))


(defun nano-agenda--center-string (string size)
  (let* ((padding (/ (- size (length string)) 2))
         (lpad (+ (length string) padding))
         (lformat (format "%%%ds" lpad))
         (rformat (format "%%%ds" (- size))))
    (format rformat (format lformat string))))


(defun nano-agenda-select-window ()
  "Function to select where to show agenda. Default
behavior is to split vertically current window.

          (before)                        (after) 
+--------------------------+    +--------------------------+
|                          |    |                          |
|                          |    |                          |
|                          | -> |                          |
|                          |    +----------+---------------+
|                          |    | calendar | agenda        |
+--------------------------+    +----------+---------------+"
  
  (split-window nil -10 nil))

(defun nano-agenda-select-entry (entry &optional date)
  "Function to decide whether an entry is
displayed/counted. Default behavior is to select only timestamped
entries."
  (get-text-property 0 'time-of-day entry))

(defun nano-agenda-default-sort-function (entries)
  "Function to decide the order ENTRIES will be shown to the user.
Returns entries in `time-of-day' order."
  (sort entries #'(lambda (entry-1 entry-2)
                    (<
                     (get-text-property 0 'time-of-day entry-1)
                     (get-text-property 0 'time-of-day entry-2)))))

(defun nano-agenda-display-entry (entry)
  "Function to display a specific (org) entry"

  (let* ((text        (get-text-property 0 'txt entry))
         (time        (get-text-property 0 'time entry))
         (time-of-day (get-text-property 0 'time-of-day entry))
         (hours       (if time-of-day
                          (format "/%02dh —/" (floor (/ time-of-day 100)))
                        "     "))
         (minutes     (if time-of-day
                          (% time-of-day 100) -1))
         (duration    (get-text-property 0 'duration entry)))
    (insert (format "%s %s\n" hours text))))


;;;###autoload
(defun nano-agenda ()
  "Create windows & buffers associated with the agenda."

  (interactive)
  
  (let* ((calendar-buffer "*nano-calendar*")
         (agenda-buffer "*nano-agenda*")
         (calendar-window (get-buffer-window calendar-buffer))
         (agenda-window (get-buffer-window agenda-buffer)))

    ;; Create calendar window if necessary
    (unless calendar-window
      (when agenda-window
        (delete-window agenda-window)
        (setq agenda-window nil))
      (setq calendar-window (nano-agenda-select-window)))

    ;; Create agenda window if necessary
    (unless agenda-window
      (setq agenda-window (split-window calendar-window 25 'right)))

    ;; Setup calendar window
   (with-selected-window calendar-window
     (switch-to-buffer calendar-buffer)
     (set-window-dedicated-p calendar-window t)
     (set-window-margins calendar-window 1)
     (nano-agenda-mode t)
     (setq header-line-format nil)
     (setq window-size-fixed 'width))

   ;; Setup agenda window
   (with-selected-window agenda-window
     (switch-to-buffer agenda-buffer)
     (org-mode)
     (nano-agenda-mode t)
     (set-window-dedicated-p agenda-window t)
     (setq header-line-format nil))

   (nano-agenda-update)
   (select-window calendar-window)
   nil))


(defun nano-agenda-update ()
  "Update calendar and agenda according to selected date."
  
  (with-current-buffer "*nano-calendar*"
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))
      (nano-agenda--populate-calendar)))

  (with-current-buffer "*nano-agenda*"
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))
      (nano-agenda--populate-agenda))))
  

(defun nano-agenda-kill ()
  "Kill buffers and windows associated with the agenda."

  (interactive)
  (let* ((calendar-buffer "*nano-calendar*")
         (agenda-buffer "*nano-agenda*"))
    (if (get-buffer calendar-buffer)
        (kill-buffer calendar-buffer))
    (if (get-buffer agenda-buffer)
        (kill-buffer agenda-buffer))))

(defun nano-agenda-refresh ()
  "Reset the cache of busy levels."

  (interactive)
  (setq nano-agenda--busy-levels (list)))


(defun nano-agenda--busy-level (date)
  "Compute the busy level at a given date. This is done by
counting the number of timed entries. Computed levels are cached
for efficiency."
    
  (let* ((day   (nano-agenda-date-day   date))
         (month (nano-agenda-date-month date))
         (year  (nano-agenda-date-year  date))
         (date  (list month day year))
         (level 0)
         (entry (assoc date nano-agenda--busy-levels)))

    (if entry
        (cadr entry)
      (progn
        (dolist (file (org-agenda-files))
          (dolist (entry (org-agenda-get-day-entries file date))
            (if (funcall nano-agenda-select-entry-predicate entry date)
                (setq level (+ level 1)))))
        (add-to-list 'nano-agenda--busy-levels `(,date ,level))
        level))))


(defun nano-agenda--populate-agenda ()
  "Populate the agenda according to current selected date."

  (let* ((selected nano-agenda--current-selection)
         (day      (nano-agenda-date-day   selected))
         (month    (nano-agenda-date-month selected))
         (year     (nano-agenda-date-year  selected))
         (date     (list month day year))
         (today    (nano-agenda-date-today))
         (is-today (nano-agenda-date-is-today selected))
         (holidays (calendar-check-holidays date))
         (entries '()))

    ;; Header (literal date + holidays (if any))
    (insert "\n")
    (insert (format-time-string "*%A %-e %B %Y*" selected))
    (if is-today
        (insert (format-time-string " /(%H:%M)/")))
    (if (and (not is-today) holidays)
        (insert (format " /(%s)/" (nth 0 holidays))))
    (insert "\n\n")

    ;; Body (default timed entries)

    ;; Collect entries from agenda files.
    (dolist (file (org-agenda-files))
      (dolist (entry (org-agenda-get-day-entries file date))
        (if (funcall nano-agenda-select-entry-predicate entry date)
            (add-to-list 'entries entry))))

    ;; Sort entries
    (setq entries (funcall nano-agenda-sort-function entries))

    ;; Display entries
    (let ((limit (if (< (length entries) 6) 6 4)))
      (dolist (entry (cl-subseq entries 0 (min limit (length entries))))
        (nano-agenda-display-entry entry))
      (if (> (length entries) limit)
          (insert (format "/+%S non-displayed event(s)/" (- (length entries) limit))))))
  
    (goto-char (point-min)))


(defun nano-agenda--populate-calendar ()
  "Populate the calendar according to the month of the current selected date."

  ;; Header with prev/next buttons
  ;; -----------------------------
  (let* ((selected nano-agenda--current-selection)
         (map-left (make-sparse-keymap))
         (map-right (make-sparse-keymap)))

    (define-key map-left (kbd "<down-mouse-1>") #'nano-agenda-backward-month)
    (define-key map-right (kbd "<down-mouse-1>") #'nano-agenda-forward-month)

    (insert "\n")
    (insert (propertize "<" 'face 'nano-agenda-button
                        'mouse-face 'nano-agenda-mouse
                        'help-echo "Previous month"
                        'keymap map-left))
    (insert (propertize (nano-agenda--center-string
                         (format "%s %d"
                                 (nano-agenda-date-month-name selected)
                                 (nano-agenda-date-year selected)) 18)
                        'face 'nano-agenda-month-name))
    (insert (propertize ">" 'face 'nano-agenda-button
                        'mouse-face 'nano-agenda-mouse
                        'help-echo "Next month"
                        'keymap map-right))
    (insert "\n")
    (insert (propertize "Mo Tu We Th Fr "
                        'face 'nano-agenda-day-name))
    (insert (propertize "Sa Su"
                        'face 'nano-agenda-day-name))
    (insert "\n"))
  
  ;; Body with navigation keymap
  ;; ---------------------------
  (let* ((selected nano-agenda--current-selection)
         (today    (nano-agenda-date-today))
         (day      (nano-agenda-date-day selected))
         (month    (nano-agenda-date-month selected))
         (year     (nano-agenda-date-year selected))
         (start    (nano-agenda-date year month 1))
         (dow      (mod (+ 6 (nano-agenda-date-dow start)) 7))
         (start    (nano-agenda-date-dec start dow)))

    (dotimes (row 6)
      (dotimes (col 7)
        (let* ((day (+ (* row 7) col))
               (date (nano-agenda-date-inc start day))

               ;; Slow
               (level (nano-agenda--busy-level date))
               (level (min (length nano-agenda-busy-foregrounds) level))
               (foreground (nth (- level 1) nano-agenda-busy-foregrounds))
               (background (nth (- level 1) nano-agenda-busy-backgrounds))
               ;; ----

               (map (make-sparse-keymap))
               (is-today (nano-agenda-date-is-today date))
               (is-selected (nano-agenda-date-equal date selected))
               (is-selected-today (and is-selected is-today))
               (is-outday (not (= (nano-agenda-date-month date) month)))
               (is-holidays (calendar-check-holidays (list
                                                      (nano-agenda-date-month date)
                                                      (nano-agenda-date-day date)
                                                      (nano-agenda-date-year date))))
               (is-weekend (or (= (nano-agenda-date-dow date) 0)
                               (= (nano-agenda-date-dow date) 6)))
               (face (cond ;; (is-selected-today 'nano-agenda-selected-today)
                           (is-selected       'nano-agenda-selected)
                           ;; (is-today          'nano-agenda-today)
                           (is-outday         'nano-agenda-outday)
                           ((> level 0)       `(:foreground ,foreground :background ,background ))
                           (is-weekend        'nano-agenda-weekend)
                           (is-holidays       'nano-agenda-holidays)
                           (t                 'nano-agenda-default))))

          (define-key map (kbd "<down-mouse-1>")
            `(lambda() (interactive) (nano-agenda-goto ,date)))

            (insert (propertize (format "%2d" (nano-agenda-date-day date))
                                'face face
                                'mouse-face (cond (is-selected-today 'nano-agenda-selected-today)
                                                  (is-selected       'nano-agenda-selected)
                                                  (t                 'nano-agenda-mouse))
                                'help-echo (format "%s%s" (format-time-string "%A %-e %B %Y" date)
                                                   (if is-holidays (format " (%s)" (nth 0 is-holidays))
                                                     ""))
                                'keymap map))
            (if (< col 6)
                (insert (propertize (if is-today nano-agenda-today-symbol " ") 'face face)))))
      (if (< row 5) (insert "\n")))))

(provide 'nano-agenda)
;;; nano-agenda.el ends here
