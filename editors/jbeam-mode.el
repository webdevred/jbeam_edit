(defvar jbeam-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\" "\"" st)
    st)
  "Syntax table for `jbeam-mode'.")

(defvar jbeam-font-lock-keywords
  `(
    ("\"\\([A-Za-z0-9_]+\\)\"\\s-*:" 1 font-lock-keyword-face)

    ("\\b[0-9]+\\(\\.[0-9]+\\)?\\b" . font-lock-constant-face)

    ("\\b\\(true\\|false\\)\\b" . font-lock-constant-face)

    ("[{}\\[\\]]" . font-lock-builtin-face)

    ("//.*$" . font-lock-comment-face)
    )
  "Highlighting for `jbeam-mode'.")

(define-derived-mode jbeam-mode prog-mode "JBeam"
  "Major mode for editing JBeam files."
  :syntax-table jbeam-mode-syntax-table
  (setq-local font-lock-defaults '(jbeam-font-lock-keywords))
  (modify-syntax-entry ?: "." jbeam-mode-syntax-table))

(add-to-list 'auto-mode-alist '("\\.jbeam\\'" . jbeam-mode))

(provide 'jbeam-mode)
