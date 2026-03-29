;;; calamars-mode.el --- major mode for calamars -*- lexical-binding: t; -*-
;;; This is a very simple mode that basically only provides basic syntax highlighting,
;;; a more complete major mode will be released once the language is actually usable :)

(defconst cm/comment-prefix "--")

;; Set of calamars keywords
(defconst cm/keywords
  '("typ"    "def"   "mut"    "given"  "match"
	"else"   "let"   "return" "module" "import"
	"trait"  "and"   "or"     "xor"    "not"
	"struct" "enum"  "true"   "false"  "if"
	"then"   "match" "case"))

;; Basic calamars types
(defconst cm/primitive-types
  '("Int" "Unit" "String" "Bool"))

(defconst cm/font-lock-def
  (append
   (mapcar (lambda (x)
             (cons (concat "\\_<" (regexp-quote x) "\\_>")
                   'font-lock-keyword-face))
           cm/keywords)
   (mapcar (lambda (x)
             (cons (concat "\\_<" (regexp-quote x) "\\_>")
                   'font-lock-type-face))
           cm/primitive-types)))

(define-derived-mode calamars-mode
  prog-mode
  "calamars"
  "Major mode for calamars files."
  (setq font-lock-defaults '((cm/font-lock-def)))
  (setq-local comment-start cm/comment-prefix))

