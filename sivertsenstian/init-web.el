;; -*- lexical-binding: t; -*-
;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - WEB MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(defun setup-tide-mode ()
  "Set up Tide mode."
  (interactive)
  (tide-setup)
  (tide-format-before-save)
  (tide-hl-identifier-mode +1)
  (eldoc-mode +1)
  (electric-pair-mode)
  (electric-quote-mode)
  (setq tide-completion-detailed t))

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
	    :n "t" #'js2-mode-toggle-element)
	  (:desc "modes" :prefix "m" :n "a" #'angular-mode)))
  (setq js2-basic-offset 2)
  (define-key evil-insert-state-map (kbd "C-d") nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

(use-package tide
  :after (:any web-mode rjsx-mode typescript-mode angular-mode)
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
  (flycheck-add-mode 'typescript-tslint 'ng2-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'angular-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode))

(use-package add-node-modules-path
  :straight t)

(use-package prettier-js
  :after add-node-modules-path
  :straight t
  :hook (rjsx-mode . add-node-modules-path) ;;use project prettier
  :hook (rjsx-mode . prettier-js-mode)
  :hook (ng2-ts-mod . add-node-modules-path) ;;use project prettier
  :hook (ng2-ts-mode . prettier-js-mode)
  :hook (ng2-html-mode . add-node-modules-path) ;;use project prettier
  :hook (ng2-html-mode . prettier-js-mode)
  :hook (less-css-mode . add-node-modules-path) ;;use project prettier
  :hook (less-css-mode . prettier-js-mode))

(use-package js2-refactor
  :after rjsx-mode
  :straight t
  :config
  (add-hook 'rjsx-mode-hook #'js2-refactor-mode))

(use-package ng2-mode
  :straight t
  :config
  (map! :map ng2-ts-mode-map
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
	  :n  "n" #'ng2-ts-goto-fn
	  :n  "c" #'ng2-open-counterpart))
  (map! :map ng2-html-map
	(:localleader
	  :nv "j" #'ng2-html-goto-binding
	  :nv "c" #'ng2-open-counterpart
	  :nv "m" #'sgml-skip-tag-forward
	  :nv "M" #'sgml-skip-tag-backward)))

(use-package angular-mode
  :straight t
  :defer t
  :config
  (electric-pair-mode)
  (electric-quote-mode)
  (map! :map angular-mode
	(:localleader
	  (:desc "modes" :prefix "m" :n "r" #'rjsx-mode))))

(use-package typescript-mode
  :mode "\\.ts$"
  :straight t
  :config
  (map! :map typescript-mode-map
	(:localleader
	  :n  "j" #'tide-jump-to-definition
	  :n  "J" #'tide-jump-to-implementation
	  :n  "b" #'tide-jump-back
	  :n  "d" #'tide-documentation-at-point
	  :n  "r" #'tide-rename-symbol
	  :n  "g" #'tide-nav
	  :n  "i" #'tide-organize-imports
	  :n  "u" #'tide-references
	  :n  "f" #'tide-fix)))

(use-package emmet-mode
  :straight t
  :config
  (add-hook 'angular-html-mode #'emmet-mode)
  (add-hook 'ng2-html-mode-hook #'emmet-mode)
  (add-hook 'rjsx-mode-hook #'emmet-mode)
  (add-hook 'less-css-mode #'emmet-mode))

;; export
(provide 'init-web) 
