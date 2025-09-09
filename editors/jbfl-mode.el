(defvar jbfl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?* ". 23" st)
    st)
  "Syntax table for `jbfl-mode'.")

(defvar jbfl-font-lock-keywords
  `(
    ("\\b\\([A-Za-z0-9_]+\\)\\s-*:" 1 font-lock-keyword-face)

    (":\\s-*\\([^;]+\\);" 1 font-lock-string-face)

    ("\\b[0-9]+\\b" . font-lock-constant-face)

    ("\\b\\(true\\|false\\)\\b" . font-lock-constant-face)

    ("\\(\\.[a-zA-Z0-9*]+\\)+" . font-lock-function-name-face)

    ("[{}]" . font-lock-builtin-face)

    (";" . font-lock-builtin-face)
    )
  "Highlighting for `jbfl-mode`")

(define-derived-mode jbfl-mode prog-mode "Jbfl"
  "Jbfl mode."
  :syntax-table jbfl-mode-syntax-table
  (setq-local font-lock-defaults '(jbfl-font-lock-keywords)
              comment-start "//")
  (modify-syntax-entry ?: "." jbfl-mode-syntax-table))

(add-to-list 'auto-mode-alist '("\\.jbfl\\'" . jbfl-mode))

(provide 'jbfl-mode)
