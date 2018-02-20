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
       browse-kill-ring-highlight-current-entry 'pulse))

(use-package browse-kill-ring+
 :after browse-kill-ring
 :straight t)

;; export
(provide 'init-syntax)
