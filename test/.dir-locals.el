((haskell-mode
  . ((eval . (setq-local haskell-process-args-cabal-repl
                     (append '("jbeam-edit:test:jbeam-edit-test") haskell-process-args-cabal-repl))))))
