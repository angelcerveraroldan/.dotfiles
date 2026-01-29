(tool-bar-mode -1)             ; Hide the outdated icons
(scroll-bar-mode -1)           ; Hide the always-visible scrollbar
(setq inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)     ; Ask for textual confirmation instead of GUI

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Set up package.el to work with MELPA
(require 'package)
(require 'evil)
(require 'eglot)
(require 'corfu)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Make sure that emacs uses the same path as a shell would
(when (memq window-system '(mac ns x wayland))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; Mono themes
(use-package almost-mono-themes
  :config
  ;; (load-theme 'almost-mono-black t)
  ;; (load-theme 'almost-mono-gray t)
  (load-theme 'almost-mono-cream t)
  ;; (load-theme 'almost-mono-white t)
  )

(unless (package-installed-p 'evil)
  (package-install 'evil))

(evil-set-undo-system 'undo-redo) ;; enable undo and redo
(evil-mode 1)

(evil-mc-mode 1)
(global-evil-mc-mode 1)


;; Leader keymap
(defvar my/leader-map (make-sparse-keymap)
  "Keymap for Space-leader shortcuts.")

;; When we jump to a linked note in orgmode, use the same window
(with-eval-after-load 'org
  (setf (cdr (assq 'file org-link-frame-setup)) #'find-file))


;; Custom functions used lated

(defun my/open-daily-notes ()
  "Open org file for daily notes"
  (interactive)
  (find-file "~/notes/work/standup.org"))

(defun my/reload-init ()
  "Reload init file"
  (interactive)
  (load-file user-init-file))

(defun my/find-file-home ()
  "Find file starting from ~/."
  (interactive)
  (let ((default-directory "~/"))
    (call-interactively #'find-file)))

(defun my/compile-from-root ()
  "Run compile command from the root"
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'compile)))


;; Make sure that my bindings can be used inside grep buffers
(with-eval-after-load 'evil
  (dolist (m (list evil-normal-state-map   ; Normal editing
                   evil-motion-state-map   ; read-only / special buffers
                   evil-visual-state-map)) ; Selections
    (define-key m (kbd "SPC") my/leader-map))) ; Add my space keybinding

(with-eval-after-load 'evil
  (evil-define-key 'normal dired-mode-map (kbd "SPC") my/leader-map)
  (evil-define-key 'motion dired-mode-map (kbd "SPC") my/leader-map)
  (evil-define-key 'visual dired-mode-map (kbd "SPC") my/leader-map))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "SPC") my/leader-map))

;; Easily reload the init file
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
(define-key my/leader-map (kbd "f h") #'my/find-file-home)

;; Cursors
(define-key my/leader-map (kbd "c b") #'evil-mc-make-cursor-in-visual-selection-beg)
(define-key my/leader-map (kbd "c e") #'evil-mc-make-cursor-in-visual-selection-end)
(define-key my/leader-map (kbd "c n") #'evil-mc-make-and-goto-next-match)
(define-key my/leader-map (kbd "c u") #'evil-mc-undo-last-added-cursor)
(define-key my/leader-map (kbd "c q") #'evil-mc-undo-all-cursors)




;; Window
(define-key my/leader-map (kbd "w v") #'split-window-right)
(define-key my/leader-map (kbd "w s") #'split-window-below)
(define-key my/leader-map (kbd "w q") #'delete-window) ; Quit
(define-key my/leader-map (kbd "w o") #'delete-other-windows)
;; Windows - Move between them
(define-key my/leader-map (kbd "w h") #'windmove-left)
(define-key my/leader-map (kbd "w j") #'windmove-down)
(define-key my/leader-map (kbd "w k") #'windmove-up)
(define-key my/leader-map (kbd "w l") #'windmove-right)

;; Project
(define-key my/leader-map (kbd "p f") #'project-find-file)   ; search files by name
(define-key my/leader-map (kbd "p s") #'project-find-regexp) ; search files by content
(define-key my/leader-map (kbd "p c") #'my/compile-from-root )  ; compile project

;; Search
(define-key my/leader-map (kbd "s s") #'isearch-forward)
(define-key my/leader-map (kbd "s g") #'rgrep) ; Search grep
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

 
;; Open important files
(define-key my/leader-map (kbd "o d") #'my/open-daily-notes)

;; LSP

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook #'eglot-ensure))

(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook #'eglot-ensure))

;; Stop eldoc from taking up an existing window - always start at the bottom
(setq eldoc-echo-area-using-multiple-p nil)
(with-eval-after-load 'eldoc
  (add-to-list 'display-buffer-alist
	       '("\\*eldoc\\*"
		 (display-buffer-in-side-window)
		 (side . bottom)
		 (slot . 1)
		 (window-height . 0.2))))

(define-key my/leader-map (kbd "l r") #'eglot-rename)
(define-key my/leader-map (kbd "l a") #'eglot-code-actions)
(define-key my/leader-map (kbd "l f") #'eglot-format)
(define-key my/leader-map (kbd "l d") #'xref-find-definitions)
(define-key my/leader-map (kbd "l e") #'flymake-show-buffer-diagnostics)
(define-key my/leader-map (kbd "l n") #'flymake-goto-next-error)
(define-key my/leader-map (kbd "l p") #'flymake-goto-prev-error)
(define-key my/leader-map (kbd "l h") #'eldoc-doc-buffer)

;; Auto complete
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
 '(package-selected-packages
   '(corfu evil evil-mc exec-path-from-shell magit rust-mode
	   sunburn-theme)))
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
