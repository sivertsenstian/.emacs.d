;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - THEME MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-fullscreen)
(global-hl-line-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(set-frame-font "Source Code Pro" nil t)
(electric-indent-mode +1)
(fringe-mode '(8 . 4))
(display-time-mode)
(set-default 'truncate-lines t)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title
   "Build a man a fire, and he'll be warm for a day. Set a man on fire, and he'll be warm for the rest of his life.")
  (setq dashboard-startup-banner (expand-file-name "sivertsenstian/logo.png" user-emacs-directory))
  (setq dashboard-items '((recents  . 5)
                          (projects . 5))))

(use-package nlinum
 :straight t
 :config
 (global-nlinum-mode))

(use-package nlinum-hl
 :after nlinum
 :straight t)

(use-package hl-todo
 :straight t
 :config
 (global-hl-todo-mode t))

(use-package all-the-icons
 :straight t)

(use-package nav-flash
 :straight t)

(use-package neotree
 :straight t
 :config
 (setq neo-theme 'icons))

(use-package all-the-icons-ivy
  :straight t
  :after ivy
  :config
  (all-the-icons-ivy-setup))

(use-package smart-mode-line
:straight t
:config
(setq evil-normal-state-tag " NORMAL")
(setq evil-insert-state-tag " INSERT")
(setq evil-visual-state-tag " VISUAL")
(setq sml/no-confirm-load-theme t
      sml/hidden-modes t
      sml/mode-width 'full)
(sml/setup))

;; THEMES
(use-package doom-themes
 :straight t)

;; (use-package spacemacs-theme
;;  :straight t)

(use-package material-theme
 :straight t)

(use-package moe-theme
 :straight t)

(use-package ample-theme
 :straight t)

(use-package nord-theme
 :straight t
 :config
 (setq nord-comment-brightness 20)
 (setq nord-region-highlight "frost")
 (load-theme 'nord t))

(use-package solarized-theme
 :straight t
 :config
 (setq solarized-emphasize-indicators nil)
 (setq solarized-use-less-bold t)
 (setq solarized-use-variable-pitch nil)
 (setq solarized-height-minus-1 1.0)
 (setq solarized-height-plus-1 1.0)
 (setq solarized-height-plus-2 1.0)
 (setq solarized-height-plus-3 1.0)
 (setq solarized-height-plus-4 1.0)
 ;(load-theme 'solarized-dark t)
 )

;; export
(provide 'init-themes)
