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

;; Local lisp directory for custom packages and config modules
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Adaptive GC: high threshold during activity, collect during idle.
;; Replaces the static 16 MB reset that used to live in early-init.el.
(use-package gcmh
  :config
  (gcmh-mode 1))

;; -----------------------------------------------------------------------------
;; Config modules (see lisp/*.el)
;; -----------------------------------------------------------------------------
;; Load order matters: cfg-evil defines `my/leader', which cfg-org, cfg-diary,
;; and cfg-dev use to register their own keybindings.
(require 'cfg-core)       ; defaults, perf tweaks, helper functions, which-key
(load (expand-file-name "theme.el" user-emacs-directory) 'noerror)
(require 'cfg-completion) ; vertico, orderless, marginalia, consult, embark, corfu
(require 'cfg-evil)       ; evil + leader key + core keybindings
(require 'cfg-org)        ; org, org-modern, org-roam
(require 'cfg-diary)      ; work diary system
(require 'cfg-dev)        ; magit, eglot, treesit-auto, dired, ement

;; -----------------------------------------------------------------------------
;; Local overrides
;; -----------------------------------------------------------------------------
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file 'noerror 'nomessage)))

;;; init.el ends here
