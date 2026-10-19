;;; cfg-org.el --- Org mode, org-modern, and org-roam -*- lexical-binding: t -*-

;;; Commentary:
;; Core Org configuration plus org-modern (visual polish) and org-roam
;; (zettelkasten-style notes).  Depends on `my/leader' from `cfg-evil'.

;;; Code:

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

(use-package org
  :ensure nil
  :demand t
  :hook ((org-mode . visual-line-mode)
         (org-mode . auto-fill-mode))
  :custom
  (org-ellipsis " ⤵")
  (org-hide-emphasis-markers t)
  (org-src-fontify-natively t)
  (org-startup-indented t)
  (org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE")))
  (org-agenda-files (list my/notes-directory
                         (expand-file-name "roam" my/notes-directory)
                         (expand-file-name "roam/issues" my/notes-directory)
                         (expand-file-name "diary" my/notes-directory)))
  (org-default-notes-file (expand-file-name "inbox.org" my/notes-directory))
  :config
  (require 'ox-md)
  (require 'org-tempo)
  ;; Open file: links in the current window instead of a new one
  (setf (cdr (assq 'file org-link-frame-setup)) #'find-file)

  (setq org-confirm-babel-evaluate nil)

  ;; fill-column for org buffers
  (add-hook 'org-mode-hook (lambda () (setq fill-column 80))))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "✸" "✿"))
  (org-modern-hide-stars t)
  (org-modern-fold-stars t))

(use-package org-roam
  :after org
  :demand t
  :init
  (setq org-roam-directory (expand-file-name "roam" my/notes-directory))
  :config
  (org-roam-db-autosync-mode 1)
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-capture-templates
        '(("n" "Note" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags:\n#+created: %U\n")
           :unnarrowed t)
          ("i" "Issue" plain "* TODO ${title} [/]
* Issue Links
- [[%^{URL to issue}][Link to issue]]
* Description
%?
* Notes and References
* Blockers
"
           :target (file+head "issues/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :issue:\n#+created: %U\n")
           :unnarrowed t)))
  :general
  (my/leader
    "n"   '(:ignore t :which-key "notes")
    "n f" '(org-roam-node-find :which-key "find node")
    "n i" '(org-roam-node-insert :which-key "insert node")
    "n c" '(org-roam-capture :which-key "capture")
    "n r" '(org-footnote-action :which-key "footnote")))

(provide 'cfg-org)
;;; cfg-org.el ends here
