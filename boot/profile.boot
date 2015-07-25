(set-env! :dependencies '[[boot-deps "0.1.3"]])

(require '[boot-deps :refer [ancient]])

(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.9.0-SNAPSHOT"]])

(swap! boot.repl/*default-middleware*
       conj 'cider.nrepl/cider-middleware)
