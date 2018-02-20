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

;; export
(provide 'init-files)
