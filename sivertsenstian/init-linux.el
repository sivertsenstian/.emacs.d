;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - LINUX SPECIFIC MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-init))

;;TODO: This is reducing performance on windows drastically all of a sudden, investigate!
(use-package evil-goggles
  :after evil
  :straight t
  :config
  (setq evil-goggles-blocking-duration 0.100)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(provide 'init-linux)
