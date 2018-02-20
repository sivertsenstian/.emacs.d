(load-file (expand-file-name "sivertsenstian/utilities.el" user-emacs-directory))
;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - CLOJURE MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package clojure-mode
 :straight t
 :config
 (map!
  (:leader
   (:desc "major" :prefix "m"
    (:desc "eval" :prefix "e"
      :desc "eval sexp"     :nv "e" #'cider-eval-last-sexp
      :desc "eval func"     :nv "f" #'cider-eval-defun-at-point
      :desc "macroexpand"   :nv "m" #'cider-macroexpand-1)
     :desc "cider jack-in" :nv "'"  #'cider-jack-in ))))

(use-package cider
 :after clojure-mode
 :straight t)

(use-package rainbow-delimiters
 :straight t
 :config
 (rainbow-delimiters-mode))

(use-package paredit
 :after clojure-mode
 :straight t
 :config
 (autoload 'enable-paredit-mode "paredit"))

(use-package smartparens
:after clojure-mode
:straight t
:config
(smartparens-global-strict-mode)
(smartparens-global-mode))

(use-package flycheck-joker
 :after flycheck
 :straight t)

;; export
(provide 'init-clojure)
