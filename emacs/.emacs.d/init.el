(tool-bar-mode -1)             ; Hide the outdated icons
(scroll-bar-mode -1)           ; Hide the always-visible scrollbar
(setq inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)     ; Ask for textual confirmation instead of GUI

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Dark mode
(load-theme 'tango-dark)

;; Set up package.el to work with MELPA
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(setq evil-undo-system 'undo-redo) ; TODO: Redo not working properly
(evil-mode 1)

;; Leader keymap
(defvar my/leader-map (make-sparse-keymap)
  "Keymap for Space-leader shortcuts.")

;; Make sure that my bindings can be used inside grep buffers
(with-eval-after-load 'evil
  (dolist (m (list evil-normal-state-map   ; Normal editing
                   evil-motion-state-map   ; read-only / special buffers
                   evil-visual-state-map)) ; Selections
    (define-key m (kbd "SPC") my/leader-map))) ; Add my space keybinding

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "SPC") my/leader-map))

;; Easily reload the init file
(defun my/reload-init ()
  "Reload init file"
  (interactive)
  (load-file user-init-file))
(define-key my/leader-map (kbd "r r") #'my/reload-init)

;; Open a terminal buffer
(defun my/terminal ()
  "Open a terminal with the default shell"
  (interactive)
  (ansi-term (getenv "SHELL")))
(define-key my/leader-map (kbd "b t") #'my/terminal)

;; Buffer
(define-key my/leader-map (kbd "b w") #'save-buffer) ; save
(define-key my/leader-map (kbd "b q") #'kill-buffer) ; quit the buffer - not the window!


;; File
(define-key my/leader-map (kbd "f t") #'dired)     ; Open file tree
(define-key my/leader-map (kbd "f s") #'find-file) ; Search files

(defun my/find-file-home ()
  "Find file starting from ~/."
  (interactive)
  (let ((default-directory "~/"))
    (call-interactively #'find-file)))
(define-key my/leader-map (kbd "f h") #'my/find-file-home)


;; Window
(define-key my/leader-map (kbd "w v") #'split-window-right)
(define-key my/leader-map (kbd "w s") #'split-window-below)
(define-key my/leader-map (kbd "w d") #'delete-window)
(define-key my/leader-map (kbd "w o") #'delete-other-windows)
;; Windows - Move between them
(define-key my/leader-map (kbd "w h") #'windmove-left)
(define-key my/leader-map (kbd "w j") #'windmove-down)
(define-key my/leader-map (kbd "w k") #'windmove-up)
(define-key my/leader-map (kbd "w l") #'windmove-right)

;; Project
(define-key my/leader-map (kbd "p f") #'project-find-file)   ; search files by name
(define-key my/leader-map (kbd "p s") #'project-find-regexp) ; search files by content
(define-key my/leader-map (kbd "p c") #'compile)             ; compile project

;; Search
(define-key my/leader-map (kbd "s s") #'isearch-forward)
(define-key my/leader-map (kbd "s g") #'rgrep) ; Search grep
(define-key my/leader-map (kbd "s n") #'next-error) ; Next result of the search
(define-key my/leader-map (kbd "s p") #'previous-error) ; Last search result

;; Git
(with-eval-after-load 'magit
  (define-key my/leader-map (kbd "g g") #'magit-status)
  (define-key my/leader-map (kbd "g c") #'magit-commit-create)
  (define-key my/leader-map (kbd "g l") #'magit-log-current)
  (define-key my/leader-map (kbd "g b") #'magit-branch)
  (define-key my/leader-map (kbd "g p") #'magit-push-current)
  (define-key my/leader-map (kbd "g f") #'magit-fetch))


;; General
(define-key my/leader-map (kbd "x") #'execute-extended-command)

;; LSP
(require 'eglot)
(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook #'eglot-ensure))


(define-key my/leader-map (kbd "l r") #'eglot-rename)
(define-key my/leader-map (kbd "l a") #'eglot-code-actions)
(define-key my/leader-map (kbd "l f") #'eglot-format)
(define-key my/leader-map (kbd "l d") #'xref-find-definitions)
(define-key my/leader-map (kbd "l e") #'flymake-show-buffer-diagnostics)
(define-key my/leader-map (kbd "l h") #'eldoc)

;; Auto complete
(require 'corfu)
(setq corfu-auto t) ; popup automatically
(global-corfu-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("545827c17d917c9bd2421f9a8d20e93ad628f5f102dbd710db86e8ddbc800b4a"
     default))
 '(package-selected-packages '(corfu evil magit rust-mode sunburn-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((t (:inherit default))))
 '(font-lock-comment-face ((t (:inherit default :foreground "gray50"))))
 '(font-lock-constant-face ((t (:inherit default))))
 '(font-lock-function-name-face ((t (:inherit default :weight bold))))
 '(font-lock-keyword-face ((t (:inherit default :weight bold))))
 '(font-lock-string-face ((t (:inherit default))))
 '(font-lock-type-face ((t (:inherit default :weight bold))))
 '(font-lock-variable-name-face ((t (:inherit default)))))
