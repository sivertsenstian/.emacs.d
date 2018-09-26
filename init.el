;;----------------------------------------------------------------------------
;; Init straight, use-package and add subdir for custom packages
;;----------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "sivertsenstian/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "sivertsenstian/themes/" user-emacs-directory)) 
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
(setq straight-cache-autoloads t
      straight-check-for-modifications 'never)
(use-package init-benchmark) ;; Measure startup time
;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(defvar sivertsenstian--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 402653184 ;;500000000
      gc-cons-percentage 1.0
      ;; consulted on every `require', `load' and various file reading
      ;; functions. You get a minor speed up by nooping this.
      file-name-handler-alist nil)
  (add-hook 'after-init-hook
	    (lambda ()
	     (setq gc-cons-threshold 16777216
		   gc-cons-percentage 0.15)))
  (add-hook 'after-init-hook
	     (lambda ()
	      (setq file-name-handler-alist sivertsenstian--file-name-handler-alist)))
;;----------------------------------------------------------------------------
;; Top-level configuration
;;----------------------------------------------------------------------------
;; (setq debug-on-error t)
(winner-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
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
  (load-theme 'doom-one t))
(use-package init-syntax)
(use-package init-bindings)
(use-package init-helm)
(use-package init-files)
(use-package init-navigation)
(use-package init-git)
(use-package init-clojure)
(use-package init-css)
;; (use-package init-elm)
(use-package init-javascript)
(use-package init-csharp)
(use-package init-hydra)
(use-package init-shell)
(use-package init-log)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-annotate-completion-candidates t)
 '(cider-completion-use-context t)
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(font-lock-maximum-size 1024000)
 '(helm-ag-base-command "rg --no-heading --vimgrep")
 '(helm-ag-insert-at-point (quote symbol))
 '(jit-lock-chunk-size 1000)
 '(safe-local-variable-values
   (quote
    ((projectile-project-compile-cmd . "\"$(cygpath -u \"$(../../tools/vswhere.exe -latest -products \"*\" -requires Microsoft.Component.MSBuild -property installationPath)\")\"/MSBuild/15.0/Bin/MSBuild.exe IRIS.OpenLab.sln //v:Minimal //nologo //p:Configuration=Release //p:Platform=x86")
     (projectile-project-compilation-cmd . "\"$(cygpath -u \"$(../../tools/vswhere.exe -latest -products \"*\" -requires Microsoft.Component.MSBuild -property installationPath)\")\"/MSBuild/15.0/Bin/MSBuild.exe IRIS.OpenLab.sln //v:Minimal //nologo //p:Configuration=Release //p:Platform=x86")
     (cider-lein-parameters . "with-profile +dirac repl :headless :host ::")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(powerline-active1 ((t (:background "#373844" :foreground "#f8f8f2"))))
 '(powerline-active2 ((t (:background "#373844" :foreground "#f8f8f2")))))
