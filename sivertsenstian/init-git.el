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
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

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

(use-package evil-goggles
  :after evil
  :straight t
  :config
  (setq evil-goggles-blocking-duration 0.100)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; export
(provide 'init-git)
