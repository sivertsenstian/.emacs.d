;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - Javascript MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
;; (use-package web-mode
;;   :mode "\\.js$"
;;   :straight t
;;   :config
;;   (setq-default flycheck-disabled-checkers
;;   		(append flycheck-disabled-checkers
;;   			'(javascript-jshint)))

;;   (flycheck-add-mode 'javascript-eslint 'web-mode)
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   (setq web-mode-content-types-alist
;; 	'(("jsx" . "\\.js[x]?\\'")))
;;   )

;; (use-package tern
;;   :straight t
;;   :hook (web-mode . tern-mode))

;; (use-package company-tern
;;   :straight t
;;   :config
;;   (add-to-list 'company-backends 'company-tern))

;; (use-package xref-js2
;;   :straight t)

;; (use-package prettier-js
;;   :straight t
;;   :hook (web-mode . prettier-js-mode))

;; (use-package js2-refactor
;;   :straight t
;;   :hook (web-mode . js2-refactor-mode))

(use-package emacs-js
  :straight t)

;; export
(provide 'init-javascript)
