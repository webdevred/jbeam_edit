((haskell-mode
  . ((haskell-process-type . cabal-repl)
     (eval . (setq-local haskell-process-args-cabal-repl
                         (append '("jbeam-edit:test:jbeam-language-server-test" "--project-file" "cabal.project.dev") haskell-process-args-cabal-repl))))))
