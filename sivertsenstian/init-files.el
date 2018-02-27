;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - FILE MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package dired-k
 :straight t
 :config
 (add-hook 'dired-initial-position-hook 'dired-k)
 (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

(use-package rg
 :straight t)

;; (use-package ag
;;  :straight t
;;  :init
;;  (progn
;;     (add-to-list 'ag-arguments "--hidden")
;;     (setq ag-group-matches nil))
;;  :config
;;  (setq-default ag-highlight-search t))

;;  ;;(add-to-list 'ag-arguments "--nogroup")
;;  ;;(setq ag-arguments "--nogroup")
;;  ;; Workaround for issue where the edits in `wgrep' mode always resulted in
;;  ;; (No changes to be performed)
;;  ;; https://github.com/Wilfred/ag.el/issues/119
;;  ;;(setq ag-group-matches nil)
;; ;; (setq ag-reuse-buffers nil)


(use-package  wgrep
 :straight t)

;; (use-package  wgrep-ag
;;   :straight (wgrep-ag :type git :host github :repo "mhayashi1120/Emacs-wgrep")
;;   :config
;;   (autoload 'wgrep-ag-setup "wgrep-ag")
;;   (add-hook 'ag-mode-hook 'wgrep-ag-setup))

(use-package iedit
 :straight t
 :config
 (setq-default ag-highlight-search t))

;; export
(provide 'init-files)
