;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - HELM ++ MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package helm
 :straight t
 :init
 (helm-mode 1)
 :config
 (setq helm-display-header-line nil)
 (set-face-attribute 'helm-source-header nil :height 0.1)
 (helm-autoresize-mode 1)
 (setq helm-autoresize-max-height 20)
 (setq helm-autoresize-min-height 20)

 (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
 (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))

 (add-to-list 'display-buffer-alist
             '("\\`\\*helm"
               (display-buffer-in-side-window)
               (window-height . 0.2)))

 (setq helm-display-function #'display-buffer)
 )

(use-package projectile
 :straight t
 :config
 (setq projectile-indexing-method 'alien)
 (setq projectile-enable-caching t))

(use-package helm-projectile
 :straight t
 :config
 (helm-projectile-on))

(use-package helm-ag
 :straight t)

;; export
(provide 'init-helm)
