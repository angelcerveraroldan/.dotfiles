;;; init.el --- Main Emacs configuration -*- lexical-binding: t -*-

;; -----------------------------------------------------------------------------
;; Package system (package.el + use-package)
;; -----------------------------------------------------------------------------
(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Keep Emacs Customize noise out of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(setq use-package-always-ensure t
      use-package-expand-minimally t)

;; Local lisp directory for custom packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq tags-revert-without-query t)

;; Notes directory (single source of truth for all notes/diary/roam paths)
(defvar my/notes-directory "~/notes"
  "Root directory for notes, diary, and org-roam.")

;; -----------------------------------------------------------------------------
;; Basics / UI
;; -----------------------------------------------------------------------------
;; Open links in qutebrowser
;;
;; If it is already running, a new tab will open (even if its in a diff workspace / minimised)
(setq browse-url-browser-function
      'browse-url-generic browse-url-generic-program "qutebrowser")

;; Better defaults
(setq-default tab-width 4)
(setq ring-bell-function #'ignore
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Runtime performance tweaks
(setq bidi-inhibit-bpa t                        ;; skip bidi bracket-pair scanning
      bidi-paragraph-direction 'left-to-right)  ;; no per-line bidi detection
(setq-default bidi-display-reordering nil)      ;; skip bidi reordering entirely
(setq inhibit-compacting-font-caches t          ;; prevent GC from compacting font caches
      fast-but-imprecise-scrolling t            ;; faster scroll (accept imprecise fontification)
      jit-lock-defer-time 0)                    ;; defer fontification to idle time
(global-so-long-mode 1)                         ;; protect against perf issues in files with long lines
(winner-mode 1)                                 ;; undo/redo window layout changes
(which-function-mode 1)                         ;; show current function in modeline

;; -----------------------------------------------------------------------------
;; Small helper functions
;; -----------------------------------------------------------------------------
(defun my/set-lmargin (lmargin)
  "Set left margin so that the text does not fall out of the monitor."
  (interactive (list (read-number "Left margin: ")))
  (set-window-margins nil lmargin nil))

(defun my/open-daily-notes ()
  "Open a file in the notes directory."
  (interactive)
  (let ((default-directory my/notes-directory))
    (call-interactively #'find-file)))

(defun my/reload-init ()
  "Reload init file."
  (interactive)
  (load-file user-init-file))

(defun my/find-file-home ()
  "Find file starting from ~/."
  (interactive)
  (let ((default-directory "~/"))
    (call-interactively #'find-file)))

(defun my/compile-from-root ()
  "Run `compile` from the project root if in a project."
  (interactive)
  (let ((default-directory
         (if-let ((proj (project-current)))
             (project-root proj)
           default-directory)))
    (call-interactively #'compile)))

(defun my/terminal ()
  "Open a terminal buffer."
  (interactive)
  (vterm))

;; -----------------------------------------------------------------------------
;; Compilation - ANSI color support
;; -----------------------------------------------------------------------------
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; -----------------------------------------------------------------------------
;; which-key
;; -----------------------------------------------------------------------------
(use-package which-key
  :init
  (setq which-key-idle-delay 0.35
        which-key-idle-secondary-delay 0.05
        which-key-max-description-length 40
        which-key-add-column-padding 2
        which-key-separator "  →  "
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.30
        which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-mode 1)
  (set-face-attribute 'which-key-key-face nil :weight 'bold)
  (set-face-attribute 'which-key-group-description-face nil :weight 'bold :height 1.10))

;; -----------------------------------------------------------------------------
;; Theme
;; -----------------------------------------------------------------------------
(condition-case err
    (load (expand-file-name "theme.el" user-emacs-directory))
  (error
   (message "Theme config error: %s" err)))

;; -----------------------------------------------------------------------------
;; Consult - search and navigation
;; -----------------------------------------------------------------------------
(use-package consult
  :defer t
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Make Consult play nicely with Corfu for in-buffer completion
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if (bound-and-true-p corfu-mode)
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; -----------------------------------------------------------------------------
;; Save history between restarts
;; -----------------------------------------------------------------------------
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

;; -----------------------------------------------------------------------------
;; Vertico - vertical completion UI
;; -----------------------------------------------------------------------------
(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

;; -----------------------------------------------------------------------------
;; Orderless - flexible matching regardless of order
;; -----------------------------------------------------------------------------
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (consult-location (styles orderless))
                                   (consult-grep (styles orderless))
                                   (consult-xref (styles orderless)))))

;; -----------------------------------------------------------------------------
;; Marginalia - rich annotations in the minibuffer
;; -----------------------------------------------------------------------------
(use-package marginalia
  :init
  (marginalia-mode 1))

;; -----------------------------------------------------------------------------
;; Embark - context actions on minibuffer candidates + search results
;; -----------------------------------------------------------------------------
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult))

;; -----------------------------------------------------------------------------
;; VTerm
;; -----------------------------------------------------------------------------
(use-package vterm
  :commands (vterm vterm-other-window))

;; -----------------------------------------------------------------------------
;; Spelling (requires enchant + pkgconf on Arch)
;; -----------------------------------------------------------------------------
(use-package jinx
  :custom
  (jinx-languages "en_GB")
  :config
  (global-jinx-mode 1))

;; -----------------------------------------------------------------------------
;; Evil + leader keys
;; -----------------------------------------------------------------------------
(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package avy
  :commands (avy-goto-char avy-goto-char-2 avy-goto-line avy-goto-word-1))

(use-package general
  :after evil
  :demand t
  :config
  (general-create-definer my/leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my/leader
    "SPC" '(execute-extended-command :which-key "M-x")
    "x"   '(execute-extended-command :which-key "M-x")

    "m"     '(:ignore t :which-key "[m]isc")
    "m d"   '(dictionary-lookup-definition :which-key "[d]efinition")
    "m s"   '(:ignore t :which-key "[s]pelling")
    "m s w" '(jinx-correct-word :which-key "correct [w]ord")
    "m s a" '(jinx-correct-all :which-key "correct [a]ll words")
    "m m"   '(my/set-lmargin :which-key "change left [m]argin")

    "b"   '(:ignore t :which-key "buffers")
    "b w" '(save-buffer :which-key "save")
    "b q" '(kill-buffer :which-key "kill")
    "b t" '(my/terminal :which-key "open a terminal")
    "b c" '(async-shell-command :which-key "run terminal command")
    "b b" '(consult-buffer :which-key "Switch buffers")
    "b i" '(consult-imenu :which-key "imenu (this buffer)")
    "b I" '(consult-imenu-multi :which-key "imenu (all buffers)")
    "b r" '(revert-buffer :which-key "re-read file from disk")

    "s"   '(:ignore t :which-key "search")
    "s g" '(rgrep :which-key "grep (recursive)")
    "s r" '(consult-ripgrep :which-key "ripgrep (recursive)")
    "s l" '(consult-line :which-key "search in buffer")
    "s f" '(find-file :which-key "find file")
    "s t" '(dired :which-key "dired")
    "s h" '(my/find-file-home :which-key "home files")
    "s o" '(consult-outline :which-key "outline (headings)")
    "s m" '(consult-mark :which-key "marks")
    "s j" '(evil-jump-backward :which-key "jump back")
    "s M" '(consult-global-mark :which-key "global marks")
    "s y" '(consult-yank-pop :which-key "yank history")
    "s w" '(avy-goto-word-1 :which-key "jump to word")
    "s J" '(avy-goto-char-2 :which-key "jump to 2 chars")

    "w"   '(:ignore t :which-key "windows")
    "w v" '(split-window-right :which-key "split right")
    "w s" '(split-window-below :which-key "split below")
    "w q" '(delete-window :which-key "delete window")
    "w o" '(delete-other-windows :which-key "only window")
    "w h" '(windmove-left :which-key "left")
    "w j" '(windmove-down :which-key "down")
    "w k" '(windmove-up :which-key "up")
    "w l" '(windmove-right :which-key "right")
    "w u" '(winner-undo :which-key "undo layout")
    "w U" '(winner-redo :which-key "redo layout")

    "p"   '(:ignore t :which-key "project")
    "p f" '(project-find-file :which-key "find file in project")
    "p s" '(project-find-regexp :which-key "search")
    "p c" '(my/compile-from-root :which-key "compile")
    "p i" '(consult-imenu-multi :which-key "imenu (all buffers)")
    "p b" '(consult-bookmark :which-key "bookmarks")
    "p r" '(consult-register :which-key "registers")

    "d"   '(:ignore t :which-key "diary")
    "d d" '(my/diary-today :which-key "today")
    "d y" '(my/diary-yesterday :which-key "yesterday")
    "d o" '(my/diary-date :which-key "open date")
    "d f" '(my/diary-find :which-key "find entry")
    "d r" '(my/diary-range :which-key "date range")

    "r"   '(:ignore t :which-key "reload")
    "r i" '(my/reload-init :which-key "reload init")))

;; -----------------------------------------------------------------------------
;; Better help buffers
;; -----------------------------------------------------------------------------
(use-package helpful
  :after general
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :general
  (my/leader
    "h"   '(:ignore t :which-key "help")
    "h f" '(helpful-callable :which-key "function")
    "h v" '(helpful-variable :which-key "variable")
    "h k" '(helpful-key      :which-key "key")
    "h c" '(helpful-command  :which-key "command")))

;; -----------------------------------------------------------------------------
;; Multiple cursors
;; -----------------------------------------------------------------------------
(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1)
  :general
  (my/leader
    "c"   '(:ignore t :which-key "cursors")
    "c b" '(evil-mc-make-cursor-in-visual-selection-beg :which-key "cursor beg")
    "c e" '(evil-mc-make-cursor-in-visual-selection-end :which-key "cursor end")
    "c n" '(evil-mc-make-and-goto-next-match :which-key "next match")
    "c u" '(evil-mc-undo-last-added-cursor :which-key "undo last")
    "c q" '(evil-mc-undo-all-cursors :which-key "undo all")))

;; -----------------------------------------------------------------------------
;; Corfu - in-buffer completion
;; -----------------------------------------------------------------------------
(use-package corfu
  :init
  (setq corfu-auto t
        corfu-preselect 'prompt)
  :config
  (global-corfu-mode 1))

;; -----------------------------------------------------------------------------
;; Git
;; -----------------------------------------------------------------------------
(use-package magit
  :commands (magit-status)
  :general
  (my/leader
    "g"   '(:ignore t :which-key "git")
    "g g" '(magit-status :which-key "status")
    "g c" '(magit-commit-create :which-key "commit")
    "g l" '(magit-log-current :which-key "log")
    "g b" '(magit-branch :which-key "branch")
    "g p" '(magit-push-current :which-key "push")
    "g f" '(magit-fetch :which-key "fetch")))

;; -----------------------------------------------------------------------------
;; Dired
;; -----------------------------------------------------------------------------
;; evil-collection provides excellent dired bindings:
;; - hjkl for navigation
;; - RET/l to open, h to go up
;; - d to flag for deletion, x to execute
;; - + to create directory
;; - C to copy, R to rename
;; - g r to refresh
;; - ! to run shell command
;; - o to open in other window

;; wdired: Edit filenames directly in dired buffer for mass renaming
(with-eval-after-load 'dired
  (require 'wdired)
  (setq wdired-allow-to-change-permissions t)

  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "E" 'wdired-change-to-wdired-mode)

  (general-define-key
   :states 'normal
   :keymaps 'wdired-mode-map
   "ZZ" 'wdired-finish-edit
   "ZQ" 'wdired-abort-changes))

;; -----------------------------------------------------------------------------
;; LSP (Eglot)
;; -----------------------------------------------------------------------------
(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (fset #'jsonrpc--log-event #'ignore)
  :general
  (my/leader
    "l"   '(:ignore t :which-key "lsp")
    "l r" '(eglot-rename :which-key "rename")
    "l a" '(eglot-code-actions :which-key "code actions")
    "l f" '(eglot-format :which-key "format")
    "l d" '(xref-find-definitions :which-key "definition")
    "l e" '(flymake-show-buffer-diagnostics :which-key "diagnostics")
    "l n" '(flymake-goto-next-error :which-key "next error")
    "l p" '(flymake-goto-prev-error :which-key "prev error")
    "l h" '(eldoc-doc-buffer :which-key "help")))


;; -----------------------------------------------------------------------------
;; Languages (tree-sitter)
;; -----------------------------------------------------------------------------
;; treesit-auto remaps traditional major modes to their built-in tree-sitter
;; equivalents (rust-ts-mode, go-ts-mode, python-ts-mode, etc.) and
;; auto-installs grammars on first use.
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

;; -----------------------------------------------------------------------------
;; Org Mode
;; -----------------------------------------------------------------------------
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
  (setf (cdr (assq 'file org-link-frame-setup)) #'find-file)

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
    "n c" '(org-roam-capture :which-key "capture")))

;; -----------------------------------------------------------------------------
;; Work diary - daily log files in ~/notes/diary/<date>.org
;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
;; Matrix client
;; -----------------------------------------------------------------------------
(use-package ement
  :commands (ement-connect ement-room-list))

;; -----------------------------------------------------------------------------
;; Make sure that the path is loaded correctly when in GUI / daemon mode
;; -----------------------------------------------------------------------------
(use-package exec-path-from-shell
  :if (or (daemonp) (display-graphic-p))
  :config
  (exec-path-from-shell-initialize))

;; -----------------------------------------------------------------------------
;; CoreOS VM manager
;; -----------------------------------------------------------------------------
(use-package coreos
  :ensure nil
  :commands (coreos-dispatch)
  :general
  (my/leader
    "v" '(coreos-dispatch :which-key "coreos")))

;; -----------------------------------------------------------------------------
;; Local overrides
;; -----------------------------------------------------------------------------
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file 'noerror 'nomessage)))

;;; init.el ends here
