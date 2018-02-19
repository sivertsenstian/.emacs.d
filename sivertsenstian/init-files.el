;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - FILE MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package dired+
 :straight t)

(use-package ag
 :straight t
 :config
 (setq-default ag-highlight-search t))

;; export
(provide 'init-files)
