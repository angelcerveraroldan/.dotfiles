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

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t
      use-package-expand-minimally t)

;; -----------------------------------------------------------------------------
;; Basics / UI
;; -----------------------------------------------------------------------------
(setq inhibit-splash-screen t
      use-file-dialog nil
      browse-url-browser-function #'eww-browse-url)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq-default tab-width 4)

;; Line numbers (relative)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Better defaults
(setq ring-bell-function #'ignore
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; -----------------------------------------------------------------------------
;; Small helper functions
;; -----------------------------------------------------------------------------
(defun my/open-daily-notes ()
  "Find notes ~/."
  (interactive)
  (let ((default-directory "~/notes"))
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
  (vterm (getenv "SHELL")))

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
  (set-face-attribute 'which-key-command-description-face nil :height 1.05)
  (set-face-attribute 'which-key-group-description-face nil :weight 'bold :height 1.10))


;; -----------------------------------------------------------------------------
;; Theme
;; -----------------------------------------------------------------------------
(condition-case err
    (load (expand-file-name "theme.el" user-emacs-directory))
  (error
   (message "Theme config error: %s" err)))


;; -----------------------------------------------------------------------------
;; Consult package
;; -----------------------------------------------------------------------------
(use-package consult
  :demand t)

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; -----------------------------------------------------------------------------
;; Save history between restarts
;; -----------------------------------------------------------------------------
(use-package savehist
  :init
  (savehist-mode 1))

;; -----------------------------------------------------------------------------
;; TODO
;; -----------------------------------------------------------------------------
(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

;; -----------------------------------------------------------------------------
;; Lets you find things regardless of order
;; -----------------------------------------------------------------------------
(use-package orderless
  :custom
  ;; Orderless everywhere, but keep files feeling normal-ish
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; -----------------------------------------------------------------------------
;; TODO
;; -----------------------------------------------------------------------------
(use-package marginalia
  :init
  (marginalia-mode 1))

;; Make Consult play nicely with Corfu for in-buffer completion
(with-eval-after-load 'consult
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if (bound-and-true-p corfu-mode)
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; -----------------------------------------------------------------------------
;; Embark: context actions on minibuffer candidates + search results
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
  :ensure t)

;; -----------------------------------------------------------------------------
;; Spelling
;; -----------------------------------------------------------------------------
;; For this package to work, you need to install some deps. Look at their docs,
;; if in arch based, the following is enough as of right now:
;;
;; yay -S enchant pkgconf
;; -----------------------------------------------------------------------------
(use-package jinx
  :ensure t
  :config
  (add-hook 'emacs-startup-hook #'global-jinx-mode))

;; -----------------------------------------------------------------------------
;; Evil + leader keys
;; -----------------------------------------------------------------------------
(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

;; General commands
(use-package general
  :after (evil)
  :demand t
  :config
  (general-create-definer my/leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Global leader keys (built-in commands + your functions)
  (my/leader
    "SPC" '(execute-extended-command :which-key "M-x")
    "x"   '(execute-extended-command :which-key "M-x")

    "m"     '(:ignore t :which-key "[m]isc")
    "m s"   '(:ignore t :which-key "[s]pelling")
    "m s w" '(jinx-correct-word :which-key "correct [w]ord")
    "m s a" '(jinx-correct-all :which-key "correct [a]ll words")


    "b"   '(:ignore t :which-key "buffers")
    "b w" '(save-buffer :which-key "save")
    "b q" '(kill-buffer :which-key "kill")
    "b t" '(my/terminal :which-key "open a terminal terminal")
	"b c" '(async-shell-command :which-key "run terminal command")
    "b b" '(consult-buffer :which-key "Switch buffers")
	"b i" '(consult-imenu :which-key "imenu (this buffer)")
	"b I" '(consult-imenu-multi :which-key "imenu (all buffers)")

	"s"   '(:ignore t :which-key "search")
	"s s" '(set-mark-command :which-key "save this spot (set a mark)")
	"s g" '(rgrep :which-key "grep (recursive)")
	"s r" '(consult-ripgrep :which-key "ripgrep (recursive)")
	"s l" '(consult-line :which-key "search in buffer")
    "s f" '(find-file :which-key "find file")
    "s t" '(dired :which-key "dired")
    "s h" '(my/find-file-home :which-key "home files")
	"s o" '(consult-outline :which-key "outline (headings)")
	"s m" '(consult-mark :which-key "marks")
	"s j" '(consult-jump-list :which-key "jump list")
	"s M" '(consult-global-mark :which-key "global marks")
	"s y" '(consult-yank-pop :which-key "yank history")

    "w"   '(:ignore t :which-key "windows")
    "w v" '(split-window-right :which-key "split right")
    "w s" '(split-window-below :which-key "split below")
    "w q" '(delete-window :which-key "delete window")
    "w o" '(delete-other-windows :which-key "only window")
    "w h" '(windmove-left :which-key "left")
    "w j" '(windmove-down :which-key "down")
    "w k" '(windmove-up :which-key "up")
    "w l" '(windmove-right :which-key "right")

    "p"   '(:ignore t :which-key "project")
    "p f" '(project-find-file :which-key "find file in project")
    "p s" '(project-find-regexp :which-key "search")
    "p c" '(my/compile-from-root :which-key "compile")
    "p i" '(consult-imenu :which-key "imenu")
	"p b" '(consult-bookmark :which-key "bookmarks")
	"p r" '(consult-register :which-key "registers")

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
;; Completion in buffers
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
(with-eval-after-load 'dired
  (general-define-key
   :states '(normal motion)
   :keymaps 'dired-mode-map
   :prefix "SPC d"
   "r" '(revert-buffer   :which-key "reload buffer (see new changes)")
   "d" '(dired-do-delete :which-key "delete file/directory")
   "c" '(shell-command   :which-key "run shell command from here")))


(with-eval-after-load 'dired
  ;; Ensure Evil keys behave in Dired (local map only)
  (evil-define-key 'normal dired-mode-map
    (kbd "RET") #'dired-find-file
    (kbd "h")   #'dired-up-directory
    (kbd "l")   #'dired-find-file))

;; -----------------------------------------------------------------------------
;; LSP Eglot
;; -----------------------------------------------------------------------------
(use-package eglot
  :hook ((rust-mode go-mode) . eglot-ensure)
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
;; Languages
;; -----------------------------------------------------------------------------
(use-package rust-mode)
(use-package go-mode)

;; -----------------------------------------------------------------------------
;; Org Mode
;; -----------------------------------------------------------------------------
(use-package org
  :demand t
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode))
  :config
  (with-eval-after-load 'org
	(require 'ox-md) ;; Allows to export org files as markdown using C-c C-e m {M|...}
    (setf (cdr (assq 'file org-link-frame-setup)) #'find-file)
	(setq org-modern-star '("◉" "○" "✸" "✿")
        org-modern-hide-stars t
        org-modern-fold-stars t
        org-ellipsis "…"))

  (setq org-ellipsis " ⤵"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-startup-indented t))

;; Where to look for todos
(setq org-agenda-files
	  (list "~/notes" "~/notes/roam"))

;; Where to write new todos
(setq org-default-notes-file "~/notes/inbox.org")

;; Make org mode buffers look a little prettier
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode))


(use-package org-roam
  :after org
  :demand t
  :init
  (setq org-roam-directory (expand-file-name "~/notes/roam"))
  :config
  (org-roam-db-autosync-mode 1)
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :general
  (my/leader
    "n"   '(:ignore t :which-key "notes")
    "n f" '(org-roam-node-find :which-key "find node")
    "n i" '(org-roam-node-insert :which-key "insert node")
    "n c" '(org-roam-capture :which-key "capture")))


;; -----------------------------------------------------------------------------
;; Matrix client
;; -----------------------------------------------------------------------------
(use-package ement)

;; -----------------------------------------------------------------------------
;; Make sure that the path is loaded correctly when in GUI mode
;; -----------------------------------------------------------------------------
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; -----------------------------------------------------------------------------
;; Local changes
;; -----------------------------------------------------------------------------
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file 'noerror 'nomessage)))
