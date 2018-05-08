;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - SQL MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
;; https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/sql/packages.el

(use-package sql-mode
  :mode "\\.sql$"
  :straight t)

(use-package sql-indent
  :after sql-mode
  :straight t)

;; export
(provide 'init-sql)
