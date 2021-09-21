;; -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - NAVIGATION MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package avy
  :commands (avy-goto-word-1
	     avy-goto-line
	     avy-goto-line-above
	     avy-goto-line-below
	     avy-goto-char)
  :straight t
  :config
  (setq avy-all-windows nil
        avy-background t))

(use-package smart-jump
  :commands (smart-jump-go
	     smart-jump-back
	     smart-jump-references
	     smart-jump-find-references-with-rg)
  :straight t
  :config
  (smart-jump-setup-default-registers))

(use-package dumb-jump
  :commands (dumb-jump-go
	     dumb-jump-quick-look
	     dumb-jump-back
	     dumb-jump-result-follow)
  :straight t
  :config
  (setq dumb-jump-aggressive nil
	dumb-jump-selector 'helm))

(use-package ace-window
  :commands (ace-window ace-swap-window ace-delete-window
			ace-select-window ace-delete-other-windows)
  :straight t
  :config (setq aw-background t))

(use-package winum
 :straight t
 :init
 (push '(("\\(.*\\) 0" . "winum-select-window-0") . ("\\1 0..9" . "window 0..9"))
      which-key-replacement-alist)
 (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)
 :config
 (winum-mode))

(use-package imenu-anywhere
  :commands helm-imenu-anywhere
  :straight t)

;; export
(provide 'init-navigation)
