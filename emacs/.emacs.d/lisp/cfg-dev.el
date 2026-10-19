;;; cfg-dev.el --- Version control, LSP, tree-sitter, and file management -*- lexical-binding: t -*-

;;; Commentary:
;; Magit, Eglot (LSP), tree-sitter major modes, Dired/Wdired tweaks, and the
;; Ement Matrix client.  Depends on `my/leader' from `cfg-evil'.

;;; Code:

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
  (setq dired-listing-switches "-lha")
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
;; Matrix client
;; -----------------------------------------------------------------------------
(use-package ement
  :commands (ement-connect ement-room-list))

(provide 'cfg-dev)
;;; cfg-dev.el ends here
