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
	  ))
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

(use-package web-mode
  :mode "\\.css$"
  :mode "\\.less$"
  :mode "\\.scss$"
  :straight t
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq css-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook))

 (use-package prettier-js
   :after rjsx-mode
   :straight t
   :hook (rjsx-mode . prettier-js-mode))

;; export
(provide 'init-javascript)
