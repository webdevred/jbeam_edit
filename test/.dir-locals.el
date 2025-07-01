((haskell-mode
  . ((haskell-process-type . cabal-repl)
     (eval . (setq-local haskell-process-args-cabal-repl
                     (append '("jbeam-edit:test:jbeam-edit-test") haskell-process-args-cabal-repl))))))
