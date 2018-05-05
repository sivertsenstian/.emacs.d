;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - Javascript MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
;; (use-package flycheck
;;   :straight t
;;   :config
;;   (setq-default flycheck-disabled-checker 'javascript-jshint)
;;   (setq-default flycheck-disabled-checker 'json-jsonlist)
;;   (setq-default flycheck-disabled-checker 'javascript-eslint)
;;   (setq-default flycheck-javascript-eslint-executable "eslint-project-relative")
;;   (flycheck-add-mode 'javascript-eslint 'web-mode)

;;   (defun my/use-eslint-from-node-modules ()
;;     (let* ((root (locate-dominating-file
;;     (or (buffer-file-name) default-directory)
;;       "node_modules"))
;;       (eslint (and root
;;       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;         root))))
;;       (when (and eslint (file-executable-p eslint))
;;     (setq-local flycheck-javascript-eslint-executable eslint))))
;;   (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
;;   (global-flycheck-mode))

(use-package json-mode
  :straight t)

(use-package rjsx-mode
  :straight t
  :config
  (map! :map rjsx-mode-map
	(:localleader
	  :n  "j" #'tide-jump-to-definition
	  :n  "b" #'tide-jump-back
	  :n  "d" #'tide-documentation-at-point
	  :n  "r" #'tide-rename-symbol-at-location
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
  :straight t
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'rjsx-mode-hook #'setup-tide-mode)

  ;;testing
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))

(use-package web-mode
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
   :straight t
   :hook (rjsx-mode . prettier-js-mode))

;; export
(provide 'init-javascript)
