((haskell-mode
  . ((eval . (setq-local haskell-process-args-cabal-repl
                     (append '("jbeam-edit:exe:jbeam-edit-dump-ast" "-f" "dump-ast") haskell-process-args-cabal-repl))))))
