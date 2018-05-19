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

;; (use-package rg
;;  :straight t)

(use-package  wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :straight t
  :config (setq wgrep-auto-save-buffer t))

(use-package iedit
 :straight t
 :config
 (setq-default ag-highlight-search t))

(use-package flx-ido
  :straight t
  :config
  (flx-ido-mode 1))


;; export
(provide 'init-files)
