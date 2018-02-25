;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - SYNTAX MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package company
 :straight t
 :config
 (global-company-mode))

(use-package flycheck
 :straight t
 :config
 (global-flycheck-mode)
 (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package browse-kill-ring
 :straight t
 :config
 (setq browse-kill-ring-show-preview t
       browse-kill-ring-highlight-current-entry t
       browse-kill-ring-highlight-inserted-item 'pulse))

(use-package undo-tree
 :straight t
 :config
 (global-undo-tree-mode))

(use-package rainbow-delimiters
 :straight t
 :config
 (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package lispy
 :straight t)

(use-package smartparens
 :straight t
 :config
 (add-hook 'prog-mode-hook #'smartparens-mode)
 (add-hook 'prog-mode-hook #'smartparens-strict-mode))

(use-package paredit
 :straight t
 :config
 (add-hook 'prog-mode-hook #'enable-paredit-mode))

;; export
(provide 'init-syntax)
