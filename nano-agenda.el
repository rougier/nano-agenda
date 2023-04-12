;;; nano-agenda.el --- N Λ N O agenda -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-agenda
;; Version: 0.3
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
;; Version 0.3.1
;; - Specific face and marke for deadlines
;;
;; Version 0.3.0
;; - Use of a single buffer for calendar + agenda
;; - Added palette choice for colorizing the calendar
;;
;; Version 0.2.2
;; - Better entries sorting
;; - Include non timestamped entries
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

(defcustom nano-agenda-deadline-symbol "!"
  "Symbol to show a deadline in calendar"
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

;; See https://material.io/design/color/the-color-system.html
(defvar nano-agenda-palettes
  '((red         . ("#FFEBEE" "#FFCDD2" "#EF9A9A" "#E57373" "#EF5350"
                    "#F44336" "#E53935" "#D32F2F" "#C62828" "#B71C1C"))
    (pink        . ("#FCE4EC" "#F8BBD0" "#F48FB1" "#F06292" "#EC407A"
                    "#E91E63" "#D81B60" "#C2185B" "#AD1457" "#880E4F"))
    (purple      . ("#F3E5F5" "#E1BEE7" "#CE93D8" "#BA68C8" "#AB47BC"
                    "#9C27B0" "#8E24AA" "#7B1FA2" "#6A1B9A" "#4A148C"))
    (deep-purple . ("#EDE7F6" "#D1C4E9" "#B39DDB" "#9575CD" "#7E57C2"
                    "#673AB7" "#5E35B1" "#512DA8" "#4527A0" "#311B92"))
    (indigo      . ("#E8EAF6" "#C5CAE9" "#9FA8DA" "#7986CB" "#5C6BC0"
                    "#3F51B5" "#3949AB" "#303F9F" "#283593" "#1A237E"))
    (blue        . ("#E3F2FD" "#BBDEFB" "#90CAF9" "#64B5F6" "#42A5F5"
                    "#2196F3" "#1E88E5" "#1976D2" "#1565C0" "#0D47A1"))
    (light-blue  . ("#E1F5FE" "#B3E5FC" "#81D4FA" "#4FC3F7" "#29B6F6"
                    "#03A9F4" "#039BE5" "#0288D1" "#0277BD" "#01579B"))
    (cyan        . ("#E0F7FA" "#B2EBF2" "#80DEEA" "#4DD0E1" "#26C6DA"
                    "#00BCD4" "#00ACC1" "#0097A7" "#00838F" "#006064"))
    (teal        . ("#E0F2F1" "#B2DFDB" "#80CBC4" "#4DB6AC" "#26A69A"
                    "#009688" "#00897B" "#00796B" "#00695C" "#004D40"))
    (green       . ("#E8F5E9" "#C8E6C9" "#A5D6A7" "#81C784" "#66BB6A"
                    "#4CAF50" "#43A047" "#388E3C" "#2E7D32" "#1B5E20"))
    (light-green . ("#F1F8E9" "#DCEDC8" "#C5E1A5" "#AED581" "#9CCC65"
                    "#8BC34A" "#7CB342" "#689F38" "#558B2F" "#33691E"))
    (lime        . ("#F9FBE7" "#F0F4C3" "#E6EE9C" "#DCE775" "#D4E157"
                    "#CDDC39" "#C0CA33" "#AFB42B" "#9E9D24" "#827717"))
    (yellow      . ("#FFFDE7" "#FFF9C4" "#FFF59D" "#FFF176" "#FFEE58"
                    "#FFEB3B" "#FDD835" "#FBC02D" "#F9A825" "#F57F17"))
    (amber       . ("#FFF8E1" "#FFECB3" "#FFE082" "#FFD54F" "#FFCA28"
                    "#FFC107" "#FFB300" "#FFA000" "#FF8F00" "#FF6F00"))
    (orange      . ("#FFF3E0" "#FFE0B2" "#FFCC80" "#FFB74D" "#FFA726"
                    "#FF9800" "#FB8C00" "#F57C00" "#EF6C00" "#E65100"))
    (deep-orange . ("#FBE9E7" "#FFCCBC" "#FFAB91" "#FF8A65" "#FF7043"
                    "#FF5722" "#F4511E" "#E64A19" "#D84315" "#BF360C"))
    (brown       . ("#EFEBE9" "#D7CCC8" "#BCAAA4" "#A1887F" "#8D6E63"
                    "#795548" "#6D4C41" "#5D4037" "#4E342E" "#3E2723"))
    (grey        . ("#FAFAFA" "#F5F5F5" "#EEEEEE" "#E0E0E0" "#BDBDBD"
                    "#9E9E9E" "#757575" "#616161" "#424242" "#212121"))
    (blue-grey   . ("#ECEFF1" "#CFD8DC" "#B0BEC5" "#90A4AE" "#78909C"
                    "#607D8B" "#546E7A" "#455A64" "#37474F" "#263238"))))


(defun nano-agenda-color-luminance (color)
  "Calculate the relative luminance of a color string (e.g. \"#ffaa00\", \"blue\").
Return a value between 0 and 1."
  (let* ((values (x-color-values color))
         (red (/ (car values) 256.0))
         (green (/ (cadr values) 256.0))
         (blue (/ (caddr values) 256.0)))
    (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 255)))

(defcustom nano-agenda-palette 'amber
  "Background colors to use to highlight a day in calendar
  view according to busy level."
  :type `(choice (const red)    (const pink)  (const purple)      (const deep-purple)
                 (const indigo) (const blue)  (const light-blue)  (const cyan)
                 (const teal)   (const green) (const light-green) (const lime)
                 (const yellow) (const amber) (const orange)      (const deep-orange)
                 (const brown)  (const grey)  (const blue-grey))
  :group 'nano-agenda-faces)

(defface nano-agenda-default
  '((t :inherit default))
  "Default face (for casual day)"
  :group 'nano-agenda-faces)

(defface nano-agenda-selected
  '((t :inherit default :inverse-video t))
  "Face for the selected day"
  :group 'nano-agenda-faces)

(defface nano-agenda-time
  '((t :inherit font-lock-comment-face))
  "Time face"
  :group 'nano-agenda-faces)

(defface nano-agenda-current-day
  '((t :inherit bold))
  "Current day face"
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
|                          |    +--------------------------+
|                          |    |    calendar /  agenda    |
+--------------------------+    +--------------------------+"

  (let* ((agenda-buffer "*nano-agenda*")
         (agenda-window (get-buffer-window agenda-buffer)))
    (or agenda-window (split-window nil -10 'below))))


(defun nano-agenda-select-entry (entry &optional date)
  "Function to decide whether an entry is
displayed/counted. Default behavior is to select all entries."
  (let ((type (get-text-property 0 'type entry)))
    (and (not (string-equal type "upcoming-deadline"))
         (not (string-search ":CANCELLED:" entry)))))

(defun nano-agenda-default-sort-function (entry-1 entry-2)
  "Function to decide the order ENTRIES will be shown to the user.
Returns entries in `time-of-day' order."

  (let ((time-1 (get-text-property 0 'time-of-day entry-1))
        (time-2 (get-text-property 0 'time-of-day entry-2)))
    (cond ((not time-1) t)
          ((not time-2) nil)
          (t (< time-1 time-2)))))

(defun nano-agenda-format-entry (entry)
  "Function to display a specific (org) entry"

  (let* ((is-deadline (string-equal (get-text-property 0 'type entry) "deadline"))
         (text (get-text-property 0 'txt entry))
         (text (replace-regexp-in-string ":.*:" "" text))
         (text (org-link-display-format text))
         (text (string-trim text))
         ;; (time (get-text-property 0 'time entry))
         (time-of-day (get-text-property 0 'time-of-day entry))
         (hours (when time-of-day (floor (/ time-of-day 100))))
         (minutes (when time-of-day (% time-of-day 100) -1))
         (duration (get-text-property 0 'duration entry))
         (text (if is-deadline
                   (propertize (concat "[D] " text) 'face 'org-imminent-deadline)
                 (propertize text 'face 'default))))
    (if hours
        (concat (propertize (format "%02dh" hours) 'face 'nano-agenda-time)
                " - "
                text)
      text)))


;;;###autoload
(defun nano-agenda ()
  "Create windows & buffers associated with the agenda."

  (interactive)
  
  (let* ((agenda-buffer "*nano-agenda*")
         (agenda-window (nano-agenda-select-window)))
    (select-window agenda-window)
    (switch-to-buffer agenda-buffer)
    (toggle-truncate-lines 1)
    (set-window-dedicated-p agenda-window t)
    (set-window-margins agenda-window 2 2)
    (nano-agenda-mode t)
    ;; (setq header-line-format nil)
    (setq mode-line-format nil)
    (nano-agenda-update)))

(defun nano-agenda-update ()
  "Update calendar and agenda according to selected date."
  
  (with-current-buffer "*nano-agenda*"
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))
      (nano-agenda--populate-calendar)
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


(defun nano-agenda--has-deadline (date)
  "Check if a cached entry has a deadline"
    
  (let* ((day   (nano-agenda-date-day   date))
         (month (nano-agenda-date-month date))
         (year  (nano-agenda-date-year  date))
         (date  (list month day year))
         (entry (assoc date nano-agenda--busy-levels)))
    (when entry
      (nth 2 entry))))

(defun nano-agenda--busy-level (date)
  "Compute the busy level at a given date. This is done by
counting the number of timed entries. Computed levels are cached
for efficiency."
    
  (let* ((day   (nano-agenda-date-day   date))
         (month (nano-agenda-date-month date))
         (year  (nano-agenda-date-year  date))
         (date  (list month day year))
         (level 0)
         (deadline nil)
         (entry (assoc date nano-agenda--busy-levels)))
    (if entry
        (cadr entry)
      (progn
        (dolist (file (org-agenda-files))
          (dolist (entry (org-agenda-get-day-entries file date))
            (when (string-equal (get-text-property 0 'type entry) "deadline")
              (setq deadline t))
            (when (funcall nano-agenda-select-entry-predicate entry date)
                (setq level (+ level 1)))))
        (add-to-list 'nano-agenda--busy-levels `(,date ,level ,deadline))
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
    (forward-line)
    (end-of-line)
    (insert "   ")
    (insert (propertize (format-time-string "%A %-e %B %Y" selected)
                        'face 'nano-agenda-current-day))
    (end-of-line)

    (if is-today
      (insert (propertize (format-time-string " (%H:%M)")
                          'face 'nano-agenda-time))
      (when holidays
        (insert (propertize (format " %s" holidays)
                            'face 'nano-agenda-holidays))))
    (forward-line 2)
    (end-of-line)

    ;; Body (default timed entries)

    ;; Collect entries from agenda files.
    (dolist (file (org-agenda-files))
      (dolist (entry (org-agenda-get-day-entries file date))
        (if (funcall nano-agenda-select-entry-predicate entry date)
            (add-to-list 'entries entry))))

    ;; Sort entries
    (setq entries (sort entries nano-agenda-sort-function))

    ;; Display entries
    (let ((limit (if (< (length entries) 7) 7 5)))
      (dolist (entry (cl-subseq entries 0 (min limit (length entries))))
        (insert (concat "   "
                        (nano-agenda-format-entry entry)))
        (forward-line)
        (end-of-line))
      (if (> (length entries) limit)
          (insert (concat "   "
                  (format "+%S non-displayed event(s)" (- (length entries) limit))))))
  
    (goto-char (point-min))))


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
    (insert (propertize "> " 'face 'nano-agenda-button
                        'mouse-face 'nano-agenda-mouse
                        'help-echo "Next month"
                        'keymap map-right))
    (insert "\n")
    (insert (propertize "Mo Tu We Th Fr "
                        'face 'nano-agenda-day-name))
    (insert (propertize "Sa Su "
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
               (backgrounds (alist-get nano-agenda-palette nano-agenda-palettes))
               (level (min (length backgrounds) level))
               (background (nth (- level 1) backgrounds))
               (foreground (if (< (nano-agenda-color-luminance background) 0.5)
                               "white" "black"))
               (map (make-sparse-keymap))
               (is-today (nano-agenda-date-is-today date))
               (has-deadline (nano-agenda--has-deadline date))
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
            (if (< col 7)
                (insert (propertize (cond (is-today nano-agenda-today-symbol)
                                          (has-deadline nano-agenda-deadline-symbol)
                                          (t " "))
                                    'face face)))))
      (if (< row 5) (insert "\n")))))

(provide 'nano-agenda)
;;; nano-agenda.el ends here
