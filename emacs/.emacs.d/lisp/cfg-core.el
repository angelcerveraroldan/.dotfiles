;;; cfg-core.el --- Core defaults, performance tweaks, and helper commands -*- lexical-binding: t -*-

;;; Commentary:
;; Baseline editor behaviour that isn't specific to any one package: startup
;; performance tuning, sane defaults, large-file protection, and a handful of
;; small interactive helper commands used throughout the rest of the config.

;;; Code:

;; Notes directory (single source of truth for all notes/diary/roam paths)
(defvar my/notes-directory "~/notes"
  "Root directory for notes, diary, and org-roam.")

;; TAGS files change often (e.g. after a rebuild) - don't prompt on revert
(setq tags-revert-without-query t)

;; -----------------------------------------------------------------------------
;; Basics / UI
;; -----------------------------------------------------------------------------

(setq browse-url-browser-function 'browse-url-firefox)

;; Better defaults
(setq-default tab-width 4)
(setq ring-bell-function #'ignore
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Runtime performance tweaks
(setq bidi-inhibit-bpa t                        ;; skip bidi bracket-pair scanning
      bidi-paragraph-direction 'left-to-right)  ;; no per-line bidi detection
(setq-default bidi-display-reordering nil)      ;; skip bidi reordering entirely
(setq inhibit-compacting-font-caches t          ;; prevent GC from compacting font caches
      fast-but-imprecise-scrolling t            ;; faster scroll (accept imprecise fontification)
      jit-lock-defer-time 0)                    ;; defer fontification to idle time
(global-so-long-mode 1)                         ;; protect against perf issues in files with long lines
(winner-mode 1)                                 ;; undo/redo window layout changes

;; Protect against freezes when opening large files (e.g. vendored libraries
;; via go-to-definition).  fundamental-mode avoids tree-sitter AST construction,
;; font-lock, and imenu scanning that would otherwise lock up Emacs.
(defun my/maybe-use-fundamental-mode ()
  "Use fundamental-mode for very large files to prevent freezes."
  (when (> (buffer-size) (* 1 1024 1024))  ; 1 MB
    (fundamental-mode)))
(add-hook 'find-file-hook #'my/maybe-use-fundamental-mode)

;; -----------------------------------------------------------------------------
;; Small helper functions
;; -----------------------------------------------------------------------------
(defun my/set-lmargin (lmargin)
  "Set left margin so that the text does not fall out of the monitor."
  (interactive (list (read-number "Left margin: ")))
  (set-window-margins nil lmargin nil))

(defun my/open-daily-notes ()
  "Open a file in the notes directory."
  (interactive)
  (let ((default-directory my/notes-directory))
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
  (vterm))

(defun my/fcmd (fmt)
  "Return a file command function from a format string FMT.
The returned function accepts (file name ext dir) and substitutes:
  %f  absolute file path  (shell-quoted)
  %n  file name with extension
  %e  extension only
  %d  directory           (shell-quoted)"
  (lambda (file name ext dir)
    (format-spec fmt `((?f . ,(shell-quote-argument file))
                       (?n . ,name)
                       (?e . ,ext)
                       (?d . ,(shell-quote-argument dir))))))

(defvar my/file-command-alist
  `(("bu"  . ,(my/fcmd "butane --pretty --strict %n"))
    ("ign" . ,(my/fcmd "ignition-validate %f")))
  "Alist of (EXTENSION . FN) used by `my/file-command'.
Each FN has signature (file name ext dir) and returns a command string.
Use `my/fcmd' for simple format-string cases, or write a plain lambda
for conditional logic:
  (\"yaml\" . (lambda (file name ext dir)
               (if (string-match \"kube\" name)
                   (format \"kubectl apply -f %s\" (shell-quote-argument file))
                 (format \"yamllint %s\" (shell-quote-argument file)))))")

(defun my/file-command ()
  "Run a shell command on the current file, with a default based on extension.
The default is looked up from `my/file-command-alist' using the file extension.
The command is shown in the minibuffer pre-filled and fully editable before
running asynchronously (same as `async-shell-command')."
  (interactive)
  (let* ((file (or (buffer-file-name) (user-error "Buffer has no associated file")))
         (name (file-name-nondirectory file))
         (ext  (file-name-extension file))
         (dir  (file-name-directory file))
         (gen  (alist-get ext my/file-command-alist nil nil #'string=))
         (cmd  (read-shell-command "File command: "
                                   (when gen (funcall gen file name ext dir)))))
    (async-shell-command cmd)))

;; -----------------------------------------------------------------------------
;; Compilation - ANSI color support
;; -----------------------------------------------------------------------------
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

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
  (set-face-attribute 'which-key-group-description-face nil :weight 'bold :height 1.10))

;; -----------------------------------------------------------------------------
;; VTerm
;; -----------------------------------------------------------------------------
(use-package vterm
  :commands (vterm vterm-other-window))

;; -----------------------------------------------------------------------------
;; Spelling (requires enchant + pkgconf on Arch)
;; -----------------------------------------------------------------------------
(use-package jinx
  :custom
  (jinx-languages "en_GB")
  :config
  (global-jinx-mode 1))

;; -----------------------------------------------------------------------------
;; Make sure that the path is loaded correctly when in GUI / daemon mode
;; -----------------------------------------------------------------------------
(use-package exec-path-from-shell
  :if (or (daemonp) (display-graphic-p))
  :config
  (exec-path-from-shell-initialize))

(provide 'cfg-core)
;;; cfg-core.el ends here
