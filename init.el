;;----------------------------------------------------------------------------
;; Init straight, use-package and add subdir for custom packages
;;----------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "sivertsenstian/" user-emacs-directory))
;;bootstrap straight package manager
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;manually load use-package with straight
(straight-use-package 'use-package)
(use-package init-benchmark) ;; Measure startup time
;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(defvar sivertsenstian--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 500000000
      gc-cons-percentage 0.6)
  (add-hook 'after-init-hook
	    (lambda ()
	     (setq gc-cons-threshold 16777216
		   gc-cons-percentage 0.1)))
  (add-hook 'after-init-hook
	     (lambda ()
	      (setq file-name-handler-alist sivertsenstian--file-name-handler-alist)))
;;----------------------------------------------------------------------------
;; Top-level configuration
;;----------------------------------------------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )
;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(use-package utilities)
(use-package init-evil)
(use-package init-themes
 :config
 (load-theme 'gotham t))
(use-package init-syntax)
(use-package init-bindings)
(use-package init-ivy)
(use-package init-files)
(use-package init-navigation)
(use-package init-git)
(use-package init-clojure)
(use-package init-css)
