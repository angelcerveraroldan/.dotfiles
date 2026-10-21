;;; cfg-diary.el --- Daily journals in work/ and personal/ -*- lexical-binding: t -*-

;;; Commentary:
;; Daily files in work/daily/<date>.org (and optional personal/daily/) with helpers to
;; open today/yesterday/a specific date, browse entries, and view a date
;; range in a single read-only buffer.  Leader keybindings ("SPC d ...") are
;; defined in `cfg-evil'.

;;; Code:

(require 'cl-lib)

(defvar my/diary-directory (expand-file-name "work/daily" my/notes-directory)
  "Directory for daily diary org files.")

(defun my/diary--ensure-dir ()
  "Create the diary directory if it doesn't exist."
  (unless (file-directory-p my/diary-directory)
    (make-directory my/diary-directory t)))

(defun my/diary--file-for-date (date)
  "Return the diary file path for DATE (a time value)."
  (expand-file-name (format-time-string "%Y-%m-%d.org" date)
                    my/diary-directory))

(defun my/diary--open-date (date)
  "Open the diary entry for DATE. Create with template if new."
  (my/diary--ensure-dir)
  (let ((file (my/diary--file-for-date date)))
    (find-file file)
    (when (= (buffer-size) 0)
      (insert (format-time-string "#+title: %Y-%m-%d\n\n* Today\n" date))
      (save-buffer))))

(defun my/diary-today ()
  "Open today's diary entry."
  (interactive)
  (my/diary--open-date (current-time)))

(defun my/diary-yesterday ()
  "Open yesterday's diary entry, including across DST transitions."
  (interactive)
  (require 'calendar)
  (pcase-let ((`(,month ,day ,year)
               (calendar-gregorian-from-absolute
                (1- (calendar-absolute-from-gregorian
                     (calendar-current-date))))))
    (my/diary--open-date (encode-time 0 0 12 day month year))))

(defun my/diary-date (date-str)
  "Open diary entry for a specific date (prompted with org date picker)."
  (interactive (list (org-read-date nil nil nil "Diary date: ")))
  (let ((date (encode-time (parse-time-string (concat date-str " 00:00:00")))))
    (my/diary--open-date date)))

(defun my/diary-find ()
  "Browse diary entries."
  (interactive)
  (my/diary--ensure-dir)
  (let ((default-directory my/diary-directory))
    (call-interactively #'find-file)))

(defun my/diary--dates-in-range (start end)
  "Return YYYY-MM-DD dates from START through END, inclusive.
Use calendar days rather than 24-hour intervals, so DST is safe."
  (require 'calendar)
  (cl-labels ((absolute (date)
                (calendar-absolute-from-gregorian
                 (list (string-to-number (substring date 5 7))
                       (string-to-number (substring date 8 10))
                       (string-to-number (substring date 0 4))))))
    (let ((first (absolute start))
          (last (absolute end))
          (dates nil))
      (cl-loop for day from first to last do
               (let ((ymd (calendar-gregorian-from-absolute day)))
                 (push (format "%04d-%02d-%02d"
                               (nth 2 ymd) (nth 0 ymd) (nth 1 ymd)) dates)))
      (nreverse dates))))

(defun my/diary-range (start end)
  "Show diary entries from START to END date in a temporary buffer."
  (interactive
   (let ((s (org-read-date nil nil nil "Start date: "))
         (e (org-read-date nil nil nil "End date: ")))
     (list s e)))
  (my/diary--ensure-dir)
  (let ((dates (my/diary--dates-in-range start end))
        (buf (get-buffer-create
              (format "*diary %s %s to %s*"
                      (file-relative-name my/diary-directory my/notes-directory)
                      start end)))
        (found 0))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (dolist (date dates)
        (let ((file (expand-file-name (concat date ".org") my/diary-directory)))
          (when (file-exists-p file)
            (setq found (1+ found))
            (goto-char (point-max))
            (insert (format "\n* %s\n\n" date))
            (insert-file-contents-literally file)
            (goto-char (point-max))
            (insert "\n"))))
      (if (= found 0)
          (insert (format "No diary entries found between %s and %s.\n" start end))
        ;; Strip #+title/#+date lines and promote headings so each day's
        ;; * Log becomes ** Log under the * <date> heading
        (goto-char (point-min))
        (while (re-search-forward "^#\\+\\(title\\|date\\):.*\n?" nil t)
          (replace-match ""))
        (goto-char (point-min))
        ;; Add a * to every heading so * -> **, ** -> ***, etc.
        (while (re-search-forward "^\\(\\*+ \\)" nil t)
          ;; Skip the date headings we inserted (exactly "* YYYY-MM-DD")
          (unless (looking-at "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$")
            (replace-match "*\\1"))))
      (goto-char (point-min))
      (org-mode)
      (read-only-mode 1))
    (switch-to-buffer buf)))

;; Scope-specific convenience functions.  The work journal stays the default
;; for existing SPC d keybindings; personal is never created on work machines.
(defun my/diary--personal-directory ()
  "Return personal daily directory, or signal if it is not checked out."
  (unless (file-directory-p (expand-file-name "personal" my/notes-directory))
    (user-error "Personal notes are not checked out on this computer"))
  (expand-file-name "personal/daily" my/notes-directory))

(defun my/diary-personal-today ()
  "Open today's personal journal."
  (interactive)
  (let ((my/diary-directory (my/diary--personal-directory)))
    (my/diary-today)))

(defun my/diary-personal-yesterday ()
  "Open yesterday's personal journal."
  (interactive)
  (let ((my/diary-directory (my/diary--personal-directory)))
    (my/diary-yesterday)))

(defun my/diary-personal-range (start end)
  "Display personal daily notes from START to END."
  (interactive
   (let ((s (org-read-date nil nil nil "Start date: "))
         (e (org-read-date nil nil nil "End date: ")))
     (list s e)))
  (let ((my/diary-directory (my/diary--personal-directory)))
    (my/diary-range start end)))

(provide 'cfg-diary)
;;; cfg-diary.el ends here
