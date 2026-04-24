;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Defer GC during startup -- reset after init
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)   ; 16 MB
                  gc-cons-percentage 0.1)))

;; Faster subprocess reads (default 4k is a bottleneck for LSP)
(setq read-process-output-max (* 1024 1024))  ; 1 MB

;; Disable UI chrome before frames are drawn
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Don't activate packages before init.el runs
(setq package-enable-at-startup nil)

;;; early-init.el ends here
