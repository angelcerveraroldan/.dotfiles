;;; theme.el --- Theme configuration -*- lexical-binding: t -*-

;; -----------------------------------------------------------------------------
;; Theme config
;; -----------------------------------------------------------------------------

;; Register custom themes directory
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

;; almost-mono-themes is the base package; our custom themes layer on top
(use-package almost-mono-themes :defer t)

;; Load the light accent theme by default.
;; Both mono-accent-light and mono-accent-dark are available via
;; my/choose-theme (SPC x my/choose-theme) or M-x consult-theme.
(load-theme 'mono-accent-light t)

;; -----------------------------------------------------------------------------
;; Easy theme switcher
;; -----------------------------------------------------------------------------
(defun my/choose-theme ()
  "Choose a theme with live preview while navigating."
  (interactive)
  (call-interactively #'consult-theme))

(use-package nyan-mode
  :config
  (nyan-mode 1)
  (nyan-toggle-wavy-trail)
  (nyan-start-animation))

(set-frame-parameter nil 'alpha-background 95)
(add-to-list 'default-frame-alist '(alpha-background . 95))

;;; theme.el ends here
