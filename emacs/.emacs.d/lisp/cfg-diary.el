;;; cfg-diary.el --- Work diary: daily log files in ~/notes/diary -*- lexical-binding: t -*-

;;; Commentary:
;; Daily log files in `my/notes-directory'/diary/<date>.org, with helpers to
;; open today/yesterday/a specific date, browse entries, and view a date
;; range in a single read-only buffer.  Leader keybindings ("SPC d ...") are
;; defined in `cfg-evil'.

;;; Code:

(defvar my/diary-directory (expand-file-name "diary" my/notes-directory)
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
      (insert (format-time-string "#+title: %A %d %B %Y\n#+date: %Y-%m-%d\n\n* Log\n** " date))
      (save-buffer))))

(defun my/diary-today ()
  "Open today's diary entry."
  (interactive)
  (my/diary--open-date (current-time)))

(defun my/diary-yesterday ()
  "Open yesterday's diary entry."
  (interactive)
  (my/diary--open-date (time-subtract (current-time) (* 24 60 60))))

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
  "Return a list of date strings (YYYY-MM-DD) from START to END inclusive."
  (let ((dates '())
        (current (encode-time (parse-time-string (concat start " 00:00:00"))))
        (end-time (encode-time (parse-time-string (concat end " 00:00:00"))))
        (one-day (* 24 60 60)))
    (while (not (time-less-p end-time current))
      (push (format-time-string "%Y-%m-%d" current) dates)
      (setq current (time-add current one-day)))
    (nreverse dates)))

(defun my/diary-range (start end)
  "Show diary entries from START to END date in a temporary buffer."
  (interactive
   (let ((s (org-read-date nil nil nil "Start date: "))
         (e (org-read-date nil nil nil "End date: ")))
     (list s e)))
  (my/diary--ensure-dir)
  (let ((dates (my/diary--dates-in-range start end))
        (buf (get-buffer-create (format "*diary %s to %s*" start end)))
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

(provide 'cfg-diary)
;;; cfg-diary.el ends here
