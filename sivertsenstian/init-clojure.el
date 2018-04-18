;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - CLOJURE MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package clojure-mode
 :defer t
 :straight t
 :mode "\\.clj$"
 :mode ("\\.cljs$" . clojurescript-mode)
 :config
 (setq cider-repl-history-file "~/.emacs.d/cider.history.log"
       cider-repl-pop-to-buffer-on-connect nil
       cider-repl-use-clojure-font-lock t
       cider-repl-use-pretty-printing t
       cider-show-error-buffer nil)
 (map! :map clojure-mode-map
  (:localleader
     :nv "e" #'cider-eval-last-sexp
     :nv "f" #'cider-eval-defun-at-point
     :n  "B" #'cider-switch-to-repl-buffer
     :n  "b" #'cider-eval-buffer
     :n  "n" #'cider-repl-set-ns
     :n  "j" #'cider-find-var
     :n  "d" #'cider-doc
     :nv "m" #'cider-macroexpand-1
     :n  "p" #'cider-eval-sexp-at-point
     :n  "r" #'cider-eval-region
     :nv "'" #'cider-jack-in)))

(use-package cider
 :after clojure-mode
 :straight t)

(use-package flycheck-joker
 :after (clojure-mode flycheck)
 :straight t)

;; export
(provide 'init-clojure)
