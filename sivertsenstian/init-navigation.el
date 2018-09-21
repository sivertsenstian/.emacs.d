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
  (smart-jump-setup-default-registers)
  (smart-jump-register :modes '(clojure-mode)))

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

(use-package popwin
 :straight t
 :config
 (popwin-mode 1)
 (push '(rg-mode :stick t) popwin:special-display-config)
 (push "*Kill Ring*" popwin:special-display-config))

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

(use-package golden-ratio
  :straight t
  :config
  (golden-ratio-mode 1)
  (defvar golden-ratio-selected-window
    (frame-selected-window)
    "Selected window.")

  (defun golden-ratio-set-selected-window
      (&optional window)
    "Set selected window to WINDOW."
    (setq-default
     golden-ratio-selected-window (or window (frame-selected-window))))

  (defun golden-ratio-selected-window-p
      (&optional window)
    "Return t if WINDOW is selected window."
    (eq (or window (selected-window))
	(default-value 'golden-ratio-selected-window)))

  (defun golden-ratio-maybe
      (&optional arg)
    "Run `golden-ratio' if `golden-ratio-selected-window-p' returns nil."
    (interactive "p")
    (unless (golden-ratio-selected-window-p)
      (golden-ratio-set-selected-window)
      (golden-ratio arg)))

  (add-hook 'buffer-list-update-hook #'golden-ratio-maybe)
  (add-hook 'focus-in-hook           #'golden-ratio)
  (add-hook 'focus-out-hook          #'golden-ratio))

;; export
(provide 'init-navigation)
