;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - THEME MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1) ; relegate tooltips to echo area only
(scroll-bar-mode -1)
(toggle-frame-fullscreen)
(global-hl-line-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(set-frame-font "Source Code Pro" nil t)
(electric-indent-mode +1)
(fringe-mode '(8 . 4))
(display-time-mode)
(set-default 'truncate-lines t)
(set-window-fringes (minibuffer-window) 0 0 nil) ;;Disable fringes in the minibuffer window.

(setq-default
 bidi-display-reordering nil ; disable bidirectional text for tiny performance boost
 blink-matching-paren nil    ; don't blink--too distracting
 cursor-in-non-selected-windows nil  ; hide cursors in other windows
 display-line-numbers-width 3
 frame-inhibit-implied-resize t
 ;; remove continuation arrow on right fringe
 fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                              fringe-indicator-alist)
 highlight-nonselected-windows nil
 image-animate-loop t
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 max-mini-window-height 0.3
 mode-line-default-help-echo nil ; disable mode-line mouseovers
 mouse-yank-at-point t           ; middle-click paste at point, not at click
 ibuffer-use-other-window t
 resize-mini-windows 'grow-only  ; Minibuffer resizing
 show-help-function nil          ; hide :help-echo text
 split-width-threshold 160       ; favor horizontal splits
 uniquify-buffer-name-style 'forward
 use-dialog-box nil              ; always avoid GUI
 visible-cursor nil
 x-stretch-cursor nil
 ;; defer jit font locking slightly to [try to] improve Emacs performance
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil
 ;; `pos-tip' defaults
 pos-tip-internal-border-width 6
 pos-tip-border-width 1
 ;; no beeping or blinking please
 ring-bell-function #'ignore
 visible-bell nil)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; highlight matching delimiters
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(show-paren-mode 1)

;;; More reliable inter-window border
;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq-default window-divider-default-places t
              window-divider-default-bottom-width 0
              window-divider-default-right-width 1)
(window-divider-mode 1)

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
  :commands nlinum-mode
  :straight t
  :init
  (add-hook 'prog-mode-hook 'nlinum-mode))

(use-package nlinum-hl
 :after nlinum
 :straight t)

(use-package hl-todo
  :commands hl-todo-mode
  :init
  (add-hook 'prog-mode-hook 'hl-todo-mode)
  :straight t
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success)))))

(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
				   all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon
				   all-the-icons-install-fonts)
  :straight t)

(use-package nav-flash
 :straight t)

(use-package neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--with-buffer
             neo-global--window-exists-p)
 :straight t
 :config
 (setq neo-theme 'icons)
 (setq neo-window-fixed-size nil))

(use-package spaceline
  :straight t
  :config
  (spaceline-spacemacs-theme)
  (spaceline-toggle-minor-modes-off)
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
  (spaceline-helm-mode))

;; THEMES
(use-package doom-themes
  :defer t
  :straight t
  :config
  (let ((height (face-attribute 'default :height)))
    ;; for all linum/nlinum users
    (set-face-attribute 'linum nil :height height)))

(use-package nord-theme
  :defer t
  :straight t)

(use-package gruber-darker-theme
  :defer t
  :straight t)

(use-package dracula-theme
  :defer t
  :straight t)

(use-package cyberpunk-theme
  :defer t
  :straight t)

(use-package gotham-theme
 :defer t
 :straight t
 :config
 (set-face-background hl-line-face "#161E26"))

(use-package solarized-theme
 :defer t
 :straight t
 :config
 (setq solarized-emphasize-indicators nil)
 (setq solarized-use-less-bold t)
 (setq solarized-use-variable-pitch nil)
 (setq solarized-height-minus-1 1.0)
 (setq solarized-height-plus-1 1.0)
 (setq solarized-height-plus-2 1.0)
 (setq solarized-height-plus-3 1.0)
 (setq solarized-height-plus-4 1.0))

;; export
(provide 'init-themes)
