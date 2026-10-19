;;; cfg-completion.el --- Minibuffer and in-buffer completion stack -*- lexical-binding: t -*-

;;; Commentary:
;; Vertico + Orderless + Marginalia + Consult + Embark form the minibuffer
;; completion stack; Corfu handles in-buffer completion; savehist persists
;; minibuffer history between sessions.

;;; Code:

;; -----------------------------------------------------------------------------
;; Consult - search and navigation
;; -----------------------------------------------------------------------------
(use-package consult
  :defer t
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Make Consult play nicely with Corfu for in-buffer completion
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if (bound-and-true-p corfu-mode)
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; -----------------------------------------------------------------------------
;; Save history between restarts
;; -----------------------------------------------------------------------------
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

;; -----------------------------------------------------------------------------
;; Vertico - vertical completion UI
;; -----------------------------------------------------------------------------
(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

;; -----------------------------------------------------------------------------
;; Orderless - flexible matching regardless of order
;; -----------------------------------------------------------------------------
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (consult-location (styles orderless))
                                   (consult-grep (styles orderless))
                                   (consult-xref (styles orderless)))))

;; -----------------------------------------------------------------------------
;; Marginalia - rich annotations in the minibuffer
;; -----------------------------------------------------------------------------
(use-package marginalia
  :init
  (marginalia-mode 1))

;; -----------------------------------------------------------------------------
;; Embark - context actions on minibuffer candidates + search results
;; -----------------------------------------------------------------------------
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult))

;; -----------------------------------------------------------------------------
;; Corfu - in-buffer completion
;; -----------------------------------------------------------------------------
(use-package corfu
  :init
  (setq corfu-auto t
        corfu-preselect 'prompt)
  :config
  (global-corfu-mode 1))

(provide 'cfg-completion)
;;; cfg-completion.el ends here
