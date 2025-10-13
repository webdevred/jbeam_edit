;;; jbfl-mode.el --- Major mode for JBeam files -*- lexical-binding: t; -*-
;;
;; Author: August Johansson
;; URL: https://github.com/webdevred/jbeam-edit
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; BSD 3-Clause License, Copyright (c) 2025 webdevred
;; All rights reserved.
;; See https://opensource.org/licenses/BSD-3-Clause for full license text.
;;
;;; Commentary:
;;
;; This package provides a major mode `jbfl-mode` for editing jbfl configuration files used jbeam-edit.
;;
;; Features:
;; - Syntax highlighting
;; - Comment handling
;;
;;; Code:

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

    (";" . font-lock-builtin-face))
  "Highlighting for `jbfl-mode`.")

(define-derived-mode jbfl-mode prog-mode "Jbfl"
  "Jbfl mode."
  :syntax-table jbfl-mode-syntax-table
  (setq-local font-lock-defaults '(jbfl-font-lock-keywords)
              comment-start "//")
  (modify-syntax-entry ?: "." jbfl-mode-syntax-table))

(when (featurep 'markdown-mode) (add-to-list 'markdown-code-lang-modes '("jbfl" . jbfl-mode)))

(add-to-list 'auto-mode-alist '("\\.jbfl\\'" . jbfl-mode))

(provide 'jbfl-mode)
;;; jbfl-mode.el ends here
