((haskell-mode . ((haskell-process-type . cabal-repl)))
 (eval . (setq-local haskell-process-args-cabal-repl
                     (append '("--project-file" "cabal.project.dev") haskell-process-args-cabal-repl))))
