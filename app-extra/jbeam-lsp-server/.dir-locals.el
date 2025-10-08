((haskell-mode
  . ((haskell-process-type . cabal-repl)
     (eval . (setq-local haskell-process-args-cabal-repl
                         (append '("jbeam-edit:exe:jbeam-lsp-server" "--project-file" "cabal.project.dev") haskell-process-args-cabal-repl))))))
