;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - CSS MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package web-mode
  :mode "\\.css$"
  :mode "\\.less$"
  :mode "\\.scss$"
  :straight t
  :config
  (map! :map rjsx-mode-map
	(:localleader
	    :n "e" #'web-mode-dom-errors-show
	    :n "z" #'web-mode-fold-or-unfold
	    :n "j" #'web-mode-element-next
	    :n "k" #'web-mode-element-previous
	  (:desc "go" :prefix "g"
	    :n "b" #'web-mode-element-beginning
	    :n "c" #'web-mode-element-child
	    :n "p" #'web-mode-element-parent
	    :n "s" #'web-mode-element-sibling-next)
	  (:desc "refactor" :prefix "r"
	    :n "c" #'web-mode-element-clone
	    :n "d" #'web-mode-element-vanish
	    :n "k" #'web-mode-element-kill
	    :n "r" #'web-mode-element-rename
	    :n "w" #'web-mode-element-wrap)))
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq css-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package helm-css-scss
  :after web-mode
  :straight t)

(use-package rainbow-mode
  :after (:any css-mode scss-mode less-css-mode web-mode)
  :straight t)

(use-package css-mode
  :straight t
  :mode "\\.css$"
  :mode ("\\.scss$" . scss-mode)
  :config
  (map! :map scss-mode-map
  (:localleader
    :nv "b" #'+css/scss-build)))

(use-package sass-mode
  :straight t
  :mode "\\.sass$"
  :config
  (map! :map scss-mode-map
  (:localleader
    :nv "b" #'+css/sass-build)))

(use-package less-css-mode
  :straight t
  :mode ("\\.less\\'" . less-css-mode))

;; export
(provide 'init-css)
