;;; cfg-org.el --- Org mode, org-modern, and org-roam -*- lexical-binding: t -*-

;;; Commentary:
;; Org tasks and agenda, plus Org-roam nodes and capture templates.
;; The Org-roam root is ~/notes; scope directories are shared/work/personal.
;; Depends on `my/notes-directory' and `my/leader'.

;;; Code:

;; Collect tasks from work/personal, plus shared issues/projects/plans.
;; Shared knowledge notes and free-form journals stay out of the agenda.
(defun my/org-refresh-agenda-files ()
  "Refresh `org-agenda-files' for task-bearing files in this checkout."
  (interactive)
  (let (files)
    (dolist (scope '("work" "personal"))
      (let ((dir (expand-file-name scope my/notes-directory)))
        (when (file-directory-p dir)
          (dolist (file (directory-files-recursively dir "\\.org\\'"))
            (unless (string-match-p "/daily/" file)
              (push file files))))))
    (dolist (kind '("issues" "projects" "planning"))
      (let ((dir (expand-file-name (concat "shared/" kind) my/notes-directory)))
        (when (file-directory-p dir)
          (dolist (file (directory-files-recursively dir "\\.org\\'"))
            (push file files)))))
    (setq org-agenda-files (nreverse files)))
  (when (called-interactively-p 'interactive)
    (message "Agenda: %d files" (length org-agenda-files))))

(defun my/org-agenda ()
  "Show the Org agenda with the current files."
  (interactive)
  (my/org-refresh-agenda-files)
  (org-agenda nil "a"))

(defun my/org-todos ()
  "Show open tasks in the current checkout, including shared trackers."
  (interactive)
  (my/org-refresh-agenda-files)
  (org-todo-list))

(use-package org
  :ensure nil
  :demand t
  :hook (org-mode . visual-line-mode)
  :custom
  (org-directory (expand-file-name my/notes-directory))
  (org-ellipsis " ⤵")
  (org-hide-emphasis-markers t)
  (org-src-fontify-natively t)
  (org-startup-indented t)
  (org-todo-keywords '((sequence "TODO" "DOING" "WAITING" "|" "DONE" "CANCELLED")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-default-notes-file (expand-file-name "work/inbox.org" my/notes-directory))
  :config
  (require 'ox-md)
  (require 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (shell . t)))
  ;; Don't silently execute shell/Elisp blocks from synced notes.
  (setq org-confirm-babel-evaluate t)
  (setf (cdr (assq 'file org-link-frame-setup)) #'find-file)
  (add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
  (setq org-capture-templates
        (append
         `(("w" "Work inbox" entry
            (file ,(expand-file-name "work/inbox.org" my/notes-directory))
            "* TODO %?\n" :empty-lines 1))
         (when (file-directory-p (expand-file-name "personal" my/notes-directory))
           `(("p" "Personal inbox" entry
              (file ,(expand-file-name "personal/inbox.org" my/notes-directory))
              "* TODO %?\n" :empty-lines 1)))))
  (my/org-refresh-agenda-files)
  :general
  (my/leader
    "a"   '(:ignore t :which-key "agenda")
    "a a" '(my/org-agenda :which-key "agenda")
    "a t" '(my/org-todos :which-key "all TODOs")
    "a c" '(org-capture :which-key "capture to inbox")
    "a r" '(my/org-refresh-agenda-files :which-key "refresh files")))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "✸" "✿"))
  (org-modern-hide-stars t)
  (org-modern-fold-stars t))

;; Build a two-level Org-roam capture menu: type (n/i/p/l), then scope
;; (w/p/s).  Personal is omitted when it is not checked out.
(defun my/org-roam-build-capture-templates ()
  "Return minimal Org-roam templates grouped by type and scope."
  (let ((scopes (append
                 '(("w" "Work" "work"))
                 (when (file-directory-p
                        (expand-file-name "personal" my/notes-directory))
                   '(("p" "Personal" "personal")))
                 '(("s" "Shared" "shared"))))
        (types '(("n" "Note" "notes" "%?"
                  "#+title: ${title}\n#+created: %U\n")
                 ("i" "Issue" "issues" "* TODO ${title}\n\n%?\n"
                  "#+title: ${title}\n#+filetags: :issue:\n")
                 ("p" "Project" "projects"
                  "* Overview\n%?\n\n* TODOs\n\n* Notes\n"
                  "#+title: ${title}\n#+filetags: :project:\n")
                 ("l" "Plan" "planning"
                  "* Plan\n:PROPERTIES:\n:PERIOD_START: %^{Start date}u\n:PERIOD_END: %^{End date}u\n:END:\n\n%?\n"
                  "#+title: ${title}\n#+filetags: :planning:\n")))
        templates)
    (dolist (type types)
      (push (list (nth 0 type) (nth 1 type)) templates)
      (dolist (scope scopes)
        (push (list (concat (nth 0 type) (nth 0 scope))
                    (nth 1 scope) 'plain (nth 3 type)
                    :target (list 'file+head
                                  (format "%s/%s/${slug}.org"
                                          (nth 2 scope) (nth 2 type))
                                  (nth 4 type))
                    :unnarrowed t)
              templates)))
    (nreverse templates)))

(defun my/org-roam-ensure-capture-directories ()
  "Create capture folders, but only inside scopes checked out locally."
  (dolist (scope '("work" "personal" "shared"))
    (let ((root (expand-file-name scope my/notes-directory)))
      (when (file-directory-p root)
        (dolist (kind '("notes" "issues" "projects" "planning"))
          (make-directory (expand-file-name kind root) t))))))

(use-package org-roam
  :after org
  :demand t
  :init
  (setq org-roam-directory (expand-file-name my/notes-directory))
  :config
  (org-roam-db-autosync-mode 1)
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (my/org-roam-ensure-capture-directories)
  (setq org-roam-capture-templates (my/org-roam-build-capture-templates))
  :general
  (my/leader
    "n"   '(:ignore t :which-key "notes")
    "n f" '(org-roam-node-find :which-key "find node")
    "n i" '(org-roam-node-insert :which-key "insert node link")
    "n c" '(org-roam-capture :which-key "new node")
    "n r" '(org-footnote-action :which-key "footnote")))

(provide 'cfg-org)
;;; cfg-org.el ends here
