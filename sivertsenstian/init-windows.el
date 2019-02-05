;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - WINDOWS SPECIFIC MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------

;; Improves lag/responsiveness of which key on windows with custom fonts
;; https://github.com/justbur/emacs-which-key/issues/130
(setq which-key-allow-imprecise-window-fit t
      inhibit-compacting-font-caches nil)

;; improve projectile speed on windows, using fd for file indexing
(setq projectile-generic-command "fd . -0 -t f --color never"
      projectile-git-command "fd . -0 -t f --color never")

(set-frame-font "Hack 10" nil t) 

;; improve magit performance?
;; (setq exec-path (add-to-list 'exec-path "C:\Users\stian\scoop\shims\"))
;; (setenv "PATH" (concat "C:\Users\stian\scoop\shims\" (getenv "PATH")))

;; WORKAROUND https://github.com/magit/magit/issues/2395
(define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
  "Mode for showing staged and unstaged changes."
  :group 'magit-status)
(defun magit-staging-refresh-buffer ()
  (magit-insert-section (status)
    (magit-insert-untracked-files)
    (magit-insert-unstaged-changes)
    (magit-insert-staged-changes)))
(defun magit-staging ()
  (interactive)
  (magit-mode-setup #'magit-staging-mode))


(use-package rich-minority
  :straight t)
(use-package smart-mode-line-atom-one-dark-theme
  :straight t)
(use-package smart-mode-line
  :straight t
  :after smart-mode-line-atom-one-dark-theme
  :config
  (display-time)
  (setq sml/theme 'atom-one-dark
	sml/no-confirm-load-theme t)
  (sml/setup)
  (setq evil-normal-state-tag   (propertize " [N] " 'face '((:foreground "DarkGoldenrod2" )))
        evil-emacs-state-tag    (propertize " [E] " 'face '((:foreground "SkyBlue2"       )))
        evil-insert-state-tag   (propertize " [I] " 'face '((:foreground "chartreuse3"    )))
        evil-replace-state-tag  (propertize " [R] " 'face '((:foreground "chocolate"      )))
        evil-motion-state-tag   (propertize " [M] " 'face '((:foreground "plum3"          )))
        evil-visual-state-tag   (propertize " [V] " 'face '((:foreground "gray"           )))
        evil-operator-state-tag (propertize " [O] " 'face '((:foreground "sandy brown"    )))) 
  (setq rm-blacklist "")
  (setq mode-line-format
	'("%e"
	  evil-mode-line-tag
	  mode-line-front-space
	  mode-line-mule-info
	  mode-line-client
	  mode-line-modified
	  mode-line-remote
	  mode-line-frame-identification
	  mode-line-buffer-identification
	  sml/pos-id-separator
	  mode-line-position 
	  (vc-mode vc-mode)
	  sml/pre-modes-separator
	  mode-line-modes
	  mode-line-misc-info
	  mode-line-end-spaces)) )

(provide 'init-windows)
