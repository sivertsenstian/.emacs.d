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
(fringe-mode '(8 . 2))

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
  (setq dashboard-startup-banner 'logo)
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

(use-package doom-themes
 :straight t)

(use-package nord-theme
 :straight t
 :config
 (load-theme 'nord t)
 (setq nord-comment-brightness 15))

(use-package all-the-icons-ivy
  :straight t
  :after ivy
  :config
  (all-the-icons-ivy-setup))

(use-package smart-mode-line
:straight t
:config
(setq sml/no-confirm-load-theme t)
(sml/setup))

;; export
(provide 'init-themes)
