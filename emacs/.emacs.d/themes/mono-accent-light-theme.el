;;; mono-accent-light-theme.el --- Light monochromatic theme with keyword accent -*- lexical-binding: t -*-

;;; Commentary:
;; Layers on top of almost-mono-cream, adding a single muted accent color
;; for keywords.  Everything else stays monochromatic.

;;; Code:

(deftheme mono-accent-light
  "Light monochromatic theme with a subtle keyword accent.")

;; Load the base monochromatic theme underneath
(load-theme 'almost-mono-cream t)

;; Override just keywords -- this theme sits on top, so our face wins
(custom-theme-set-faces
 'mono-accent-light
 '(font-lock-keyword-face ((t :foreground "#4a6fa5" :weight bold))))

(provide-theme 'mono-accent-light)

;; Re-enable the base theme whenever this theme is enabled (e.g. after
;; switching away and back via consult-theme).  The `unless` guard
;; prevents infinite recursion: it only fires when the base is missing.
(defun mono-accent-light--ensure-base (&rest _)
  "Ensure almost-mono-cream is active under mono-accent-light."
  (when (custom-theme-enabled-p 'mono-accent-light)
    (unless (custom-theme-enabled-p 'almost-mono-cream)
      (load-theme 'almost-mono-cream t)
      (enable-theme 'mono-accent-light))))

(add-hook 'enable-theme-functions #'mono-accent-light--ensure-base)

;;; mono-accent-light-theme.el ends here
