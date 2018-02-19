;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - GIT MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package magit
 :defer t
 :straight t
 :config
 (which-key-mode))

(use-package git-gutter+
 :straight t
 :config
 (global-git-gutter+-mode)
 (set-face-background 'git-gutter+-modified "#E9CB8B")
 (set-face-background 'git-gutter+-added "#A3BE8C")
 (set-face-background 'git-gutter+-deleted "#CE8770"))

(use-package git-gutter-fringe+
 :straight t
 :config
 (setq git-gutter-fr+-side 'right-fringe))

(use-package git-timemachine
 :defer t
 :straight t)

;; export
(provide 'init-git)
