;;; cfg-evil.el --- Evil mode, leader key, and core keybindings -*- lexical-binding: t -*-

;;; Commentary:
;; Evil (+ collection/surround/mc), Avy, and General together provide modal
;; editing and the leader-key (SPC) keybinding system.  `my/leader' is
;; defined here and reused by other config modules (cfg-org, cfg-dev, ...)
;; to register their own bindings under the shared prefix, so this module
;; must be loaded before those.

;;; Code:

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)

  (defun my/surround-word ()
    "Surround the word at point with a prompted character pair.
Works regardless of cursor position within the word."
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (if bounds
          (let ((char (read-char "Surround with: ")))
            (evil-surround-region (car bounds) (cdr bounds) 'exclusive char))
        (user-error "No word at point"))))

  (evil-define-key 'normal 'global (kbd "g s") #'my/surround-word))

(use-package avy
  :commands (avy-goto-char avy-goto-char-2 avy-goto-line avy-goto-word-1))

(use-package general
  :after evil
  :demand t
  :config
  (general-create-definer my/leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my/leader
    "SPC" '(execute-extended-command :which-key "M-x")
    "x"   '(execute-extended-command :which-key "M-x")

    "m"     '(:ignore t :which-key "[m]isc")
    "m d"   '(dictionary-lookup-definition :which-key "[d]efinition")
    "m s"   '(:ignore t :which-key "[s]pelling")
    "m s w" '(jinx-correct-word :which-key "correct [w]ord")
    "m s a" '(jinx-correct-all :which-key "correct [a]ll words")
    "m m"   '(my/set-lmargin :which-key "change left [m]argin")

    "b"   '(:ignore t :which-key "buffers")
    "b w" '(save-buffer :which-key "save")
    "b q" '(kill-buffer :which-key "kill")
    "b t" '(my/terminal :which-key "open a terminal")
    "b c" '(async-shell-command :which-key "run terminal command")
    "b f" '(my/file-command :which-key "run file command")
    "b b" '(consult-buffer :which-key "Switch buffers")
    "b i" '(consult-imenu :which-key "imenu (this buffer)")
    "b I" '(consult-imenu-multi :which-key "imenu (all buffers)")
    "b r" '(revert-buffer :which-key "re-read file from disk")

    "s"   '(:ignore t :which-key "search")
    "s g" '(rgrep :which-key "grep (recursive)")
    "s r" '(consult-ripgrep :which-key "ripgrep (recursive)")
    "s l" '(consult-line :which-key "search in buffer")
    "s f" '(find-file :which-key "find file")
    "s t" '(dired :which-key "dired")
    "s h" '(my/find-file-home :which-key "home files")
    "s o" '(consult-outline :which-key "outline (headings)")
    "s m" '(consult-mark :which-key "marks")
    "s j" '(evil-jump-backward :which-key "jump back")
    "s M" '(consult-global-mark :which-key "global marks")
    "s y" '(consult-yank-pop :which-key "yank history")
    "s w" '(avy-goto-word-1 :which-key "jump to word")
    "s J" '(avy-goto-char-2 :which-key "jump to 2 chars")

    "w"   '(:ignore t :which-key "windows")
    "w v" '(split-window-right :which-key "split right")
    "w s" '(split-window-below :which-key "split below")
    "w q" '(delete-window :which-key "delete window")
    "w o" '(delete-other-windows :which-key "only window")
    "w h" '(windmove-left :which-key "left")
    "w j" '(windmove-down :which-key "down")
    "w k" '(windmove-up :which-key "up")
    "w l" '(windmove-right :which-key "right")
    "w u" '(winner-undo :which-key "undo layout")
    "w U" '(winner-redo :which-key "redo layout")

    "p"   '(:ignore t :which-key "project")
    "p f" '(project-find-file :which-key "find file in project")
    "p s" '(project-find-regexp :which-key "search")
    "p c" '(my/compile-from-root :which-key "compile")
    "p i" '(consult-imenu-multi :which-key "imenu (all buffers)")
    "p b" '(consult-bookmark :which-key "bookmarks")
    "p r" '(consult-register :which-key "registers")

    "d"   '(:ignore t :which-key "diary")
    "d d" '(my/diary-today :which-key "today")
    "d y" '(my/diary-yesterday :which-key "yesterday")
    "d o" '(my/diary-date :which-key "open date")
    "d f" '(my/diary-find :which-key "find entry")
    "d r" '(my/diary-range :which-key "date range")

    "f"   '(fill-region :which-key "[f]ill region")

    "r"   '(:ignore t :which-key "reload")
    "r i" '(my/reload-init :which-key "reload init")))

;; -----------------------------------------------------------------------------
;; Better help buffers
;; -----------------------------------------------------------------------------
(use-package helpful
  :after general
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :general
  (my/leader
    "h"   '(:ignore t :which-key "help")
    "h f" '(helpful-callable :which-key "function")
    "h v" '(helpful-variable :which-key "variable")
    "h k" '(helpful-key      :which-key "key")
    "h c" '(helpful-command  :which-key "command")))

;; -----------------------------------------------------------------------------
;; Multiple cursors
;; -----------------------------------------------------------------------------
(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1)
  :general
  (my/leader
    "c"   '(:ignore t :which-key "cursors")
    "c b" '(evil-mc-make-cursor-in-visual-selection-beg :which-key "cursor beg")
    "c e" '(evil-mc-make-cursor-in-visual-selection-end :which-key "cursor end")
    "c n" '(evil-mc-make-and-goto-next-match :which-key "next match")
    "c u" '(evil-mc-undo-last-added-cursor :which-key "undo last")
    "c q" '(evil-mc-undo-all-cursors :which-key "undo all")))

(provide 'cfg-evil)
;;; cfg-evil.el ends here
