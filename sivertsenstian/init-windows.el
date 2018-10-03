;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - WINDOWS SPECIFIC MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------

(use-package spaceline
  :straight t
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
  (spaceline-helm-mode))

(provide 'init-windows)
