;; -*- lexical-binding: t; -*-
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
	  :n  "J" #'tide-jump-to-implementation
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
  (tide-format-before-save)
  (setq tide-completion-detailed t))

(use-package tide
  :after (:any web-mode rjsx-mode typescript-mode)
  :straight t
  :config
  (setq company-tooltip-align-annotations t
	tide-completion-detailed t
        tide-always-show-documentation t)
  (add-hook 'rjsx-mode-hook #'setup-tide-mode)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (setq-default flycheck-disabled-checker 'javascript-jshint)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode))

(use-package add-node-modules-path
  :after rjsx-mode
  :straight t)

(use-package prettier-js
  :after (rjsx-mode add-node-modules-path typescript-mode)
  :straight t
  :hook (rjsx-mode . add-node-modules-path) ;;use project prettier
  :hook (rjsx-mode . prettier-js-mode)
  :hook (typescript-mode . add-node-modules-path)
  :hook (typescript-mode . prettier-js-mode)
  :hook (less-css-mode . add-node-modules-path)
  :hook (less-css-mode . prettier-js-mode))

(use-package js2-refactor
  :after rjsx-mode 
  :straight t
  :config
  (add-hook 'rjsx-mode-hook #'js2-refactor-mode))

(use-package typescript-mode
  :mode "\\.ts$"
  :straight t)

;; export
(provide 'init-javascript)
