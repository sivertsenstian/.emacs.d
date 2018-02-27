;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - FILE MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package dired-k
 :straight t
 :config
 (add-hook 'dired-initial-position-hook 'dired-k)
 (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

(use-package ag
 :straight t
 :config
 (setq-default ag-highlight-search t))

 ;;(add-to-list 'ag-arguments "--nogroup")
 ;;(setq ag-arguments "--nogroup")
 ;; Workaround for issue where the edits in `wgrep' mode always resulted in
 ;; (No changes to be performed)
 ;; https://github.com/Wilfred/ag.el/issues/119
 ;;(setq ag-group-matches nil)
;; (setq ag-reuse-buffers nil)
(use-package wgrep
 :straight t
 :config
 (setq wgrep-auto-save-buffer t))

(use-package wgrep-ag
 :straight t
 :config
 (autoload 'wgrep-ag-setup "wgrep-ag")
 (add-hook 'ag-mode-hook 'wgrep-ag-setup))

(use-package iedit
 :straight t
 :config
 (setq-default ag-highlight-search t))

;; export
(provide 'init-files)
