(load-file (expand-file-name "sivertsenstian/utilities.el" user-emacs-directory))
;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - CLOJURE MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package clojure-mode
 :straight t
  :mode "\\.clj$"
  :mode ("\\.cljs$" . clojurescript-mode)
 :config
 (map! :map clojure-mode-map
  (:localleader
     :nv "e" #'cider-eval-last-sexp
     :nv "f" #'cider-eval-defun-at-point
     :n  "j" #'cider-find-var
     :n  "d" #'cider-doc
     :nv "m" #'cider-macroexpand-1)
     :nv "'"  #'cider-jack-in))

(use-package cider
 :after clojure-mode
 :straight t)

(use-package rainbow-delimiters
 :straight t
 :config
 (rainbow-delimiters-mode))

(use-package paredit
 :straight t
 :config
 (autoload 'enable-paredit-mode "paredit"))

(use-package smartparens
:straight t
:config
(smartparens-global-strict-mode)
(smartparens-global-mode))

(use-package flycheck-joker
 :after flycheck
 :straight t)

;; export
(provide 'init-clojure)
