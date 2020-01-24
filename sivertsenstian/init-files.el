;; -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - FILE MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
;;dired
(setq ;; Always copy/delete recursively
 dired-recursive-copies  'always
 dired-recursive-deletes 'top
 ;; Auto refresh dired, but be quiet about it
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil)

(use-package dired-k
  :after dired
  :straight t
  :config
  (map! :map dired-mode-map
	:n "c" #'find-file
	:n "d" #'dired-do-delete
	:n "r" #'dired-do-rename)
  (setq dired-k-style 'git)
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

(use-package treemacs
  :straight t)

(use-package treemacs-evil
  :straight t)

(use-package treemacs-projectile
  :straight t)

;; export
(provide 'init-files)
