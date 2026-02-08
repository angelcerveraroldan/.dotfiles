;; -----------------------------------------------------------------------------
;; Theme config - Change this to make it prettier
; -----------------------------------------------------------------------------

;; Default dark and light themes - they have almost no color. Change these to the
;; themes you like the most!
(setq my/dark-theme  'almost-mono-gray)
(setq my/light-theme 'almost-mono-cream)
 
(use-package almost-mono-themes
   :config
   (load-theme my/dark-theme t))

;; -----------------------------------------------------------------------------
;; Easy theme switcher
;; -----------------------------------------------------------------------------
(defun my/choose-theme ()
  "Disable current themes and choose a new one."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (call-interactively #'load-theme))

;; Uncomment to install a lot of nice themes (space x my/choose-theme enter, then
;; you will see a lot of themes, you can choose any)
;; (use-package doom-themes)

;; Very important, uncomment the next few lines for emacs to not break!!!!!
;; (use-package nyan-mode
;;  :config
;;  (nyan-mode 1))
