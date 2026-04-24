;;; mono-accent-dark-theme.el --- Dark monochromatic theme with keyword accent -*- lexical-binding: t -*-

;;; Commentary:
;; Layers on top of almost-mono-gray, adding a single muted accent color
;; for keywords.  Everything else stays monochromatic.

;;; Code:

(deftheme mono-accent-dark
  "Dark monochromatic theme with a subtle keyword accent.")

;; Load the base monochromatic theme underneath
(load-theme 'almost-mono-gray t)

;; Override just keywords -- this theme sits on top, so our face wins
(custom-theme-set-faces
 'mono-accent-dark
 '(font-lock-keyword-face ((t :foreground "#8bacc4" :weight bold))))

(provide-theme 'mono-accent-dark)

;; Re-enable the base theme whenever this theme is enabled (e.g. after
;; switching away and back via consult-theme).  The `unless` guard
;; prevents infinite recursion: it only fires when the base is missing.
(defun mono-accent-dark--ensure-base (&rest _)
  "Ensure almost-mono-gray is active under mono-accent-dark."
  (when (custom-theme-enabled-p 'mono-accent-dark)
    (unless (custom-theme-enabled-p 'almost-mono-gray)
      (load-theme 'almost-mono-gray t)
      (enable-theme 'mono-accent-dark))))

(add-hook 'enable-theme-functions #'mono-accent-dark--ensure-base)

;;; mono-accent-dark-theme.el ends here
