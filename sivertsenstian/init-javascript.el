;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - Javascript MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package json-mode
  :mode "\\.json$"
  :straight t)

(use-package rjsx-mode
  :mode "\\.js$"
  :straight t
  :config
  (map! :map rjsx-mode-map
	(:localleader
	  :n  "j" #'tide-jump-to-definition
	  :n  "b" #'tide-jump-back
	  :n  "d" #'tide-documentation-at-point
	  :n  "r" #'tide-rename-symbol
	  :n  "g" #'tide-nav
	  :n  "i" #'tide-organize-imports
	  :n  "u" #'tide-references
	  :n  "f" #'tide-fix
	  (:desc "html" :prefix "h"
	    :n "r" #'rjsx-rename-tag-at-point
	    :n "t" #'js2-mode-toggle-element)))
  (setq js2-basic-offset 2)
  (define-key evil-insert-state-map (kbd "C-d") nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

(defun setup-tide-mode ()
  "Set up Tide mode."
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;;testing 1
  (setq tide-completion-detailed t))

(use-package tide
  :after (:any web-mode rjsx-mode)
  :straight t
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'rjsx-mode-hook #'setup-tide-mode)

  ;;testing
  (setq-default flycheck-disabled-checker 'javascript-jshint)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

 (use-package prettier-js
   :after rjsx-mode
   :straight t
   :hook (rjsx-mode . prettier-js-mode))

(use-package js2-refactor
  :after rjsx-mode 
  :straight t
  :config
  (add-hook 'rjsx-mode-hook #'js2-refactor-mode))

;; export
(provide 'init-javascript)
