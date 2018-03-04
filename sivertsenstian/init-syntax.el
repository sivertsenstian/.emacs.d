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

(use-package flycheck-color-mode-line
 :after flycheck
 :straight t
 :config
 (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

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

(use-package highlight-indentation
 :straight t
 :config
 (set-face-background 'highlight-indentation-face "#195466")
 (set-face-background 'highlight-indentation-current-column-face "#26859E"))

(use-package lispy
 :straight t)

(use-package smartparens
 :straight t
 :config
 (add-hook 'prog-mode-hook #'smartparens-mode)
 (add-hook 'prog-mode-hook #'smartparens-strict-mode))

;; export
(provide 'init-syntax)
