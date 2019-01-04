;; -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - GIT MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")

(use-package ssh-agency
  :straight t)

(use-package magit
  :commands (magit-status magit-blame)
  :straight t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (which-key-mode))

(use-package magit-todos
  :straight t)

;; TODO: Do somethign
(use-package git-gutter+
 :straight t
 :config
 (global-git-gutter+-mode)
 (set-face-foreground 'git-gutter+-modified "#33859E")
 (set-face-foreground 'git-gutter+-added "#2AA889")
 (set-face-foreground 'git-gutter+-deleted "#D25C26")
 (set-face-background 'git-gutter+-modified "#33859E")
 (set-face-background 'git-gutter+-added "#2AA889")
 (set-face-background 'git-gutter+-deleted "#D25C26"))

(use-package git-gutter-fringe+
  :straight t
  :config
  (setq git-gutter-fr+-side 'right-fringe))

(use-package git-timemachine
  :commands (git-timemachine git-timemachine-toggle)
  :defer t
  :straight t)

;; export
(provide 'init-git)
