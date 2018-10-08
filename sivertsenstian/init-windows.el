;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - WINDOWS SPECIFIC MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(setq projectile-git-command "fd . -0 --color never")

(set-frame-font "Inconsolata 12" nil t)

(use-package spaceline
  :straight t
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
  (spaceline-helm-mode))

(provide 'init-windows)
