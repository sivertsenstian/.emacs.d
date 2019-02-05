;; -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------------
;; Init straight, use-package and add subdir for custom packages
;;----------------------------------------------------------------------------
(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my initl!
      package--init-file-ensured t)

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
(setq straight-cache-autoloads t)
(use-package init-benchmark) ;; Measure startup time
;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(defvar sivertsenstian--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 402653184 
      gc-cons-percentage 0.6
      ;; consulted on every `require', `load' and various file reading
      ;; functions. You get a minor speed up by nooping this.
      file-name-handler-alist nil)
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
;; (setq debug-on-error t)
(winner-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; stop creating .#lock files
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
  (load-theme 'kaolin-ocean t)
  ;; (load-theme 'doom-one t)
  ;; (solaire-mode-swap-bg)
  )
(use-package init-syntax)
(use-package init-bindings)
(use-package init-files)
(use-package init-navigation)
(use-package init-hydra)

;; Tools
(use-package init-helm)
(use-package init-git)

;; Languages
(use-package init-css)
(use-package init-clojure)
(use-package init-elm)
(use-package init-web)
(use-package init-csharp)
(use-package init-rust)

;; init windows or linux stuff based on system-type
(if (eq system-type 'windows-nt)
    (progn
      (use-package init-windows)
      (use-package init-powershell
	:config
	(map! (:leader :desc "shell" :n "!"   #'powershell))))
  (progn (use-package init-linux)
	 (use-package init-shell
	   :config
	   (map! (:leader :desc "shell" :n "!"   #'shell)))))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-annotate-completion-candidates t)
 '(cider-completion-use-context t)
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(font-lock-maximum-size 1024000)
 '(helm-ag-base-command "rg --no-heading --vimgrep --smart-case")
 '(helm-ag-insert-at-point (quote symbol))
 '(jit-lock-chunk-size 1000)
 '(magit-diff-section-arguments (quote ("--ignore-space-change" "--no-ext-diff")))
 '(safe-local-variable-values
   (quote
    ((elm-package-json . "elm.json")
     (elm-compile-arguments "--output=elm.js" "--debug")
     (elm-reactor-arguments "--port" "8000")
     (projectile-project-compile-cmd . "\"$(cygpath -u \"$(../../tools/vswhere.exe -latest -products \"*\" -requires Microsoft.Component.MSBuild -property installationPath)\")\"/MSBuild/15.0/Bin/MSBuild.exe IRIS.OpenLab.sln //v:Minimal //nologo //p:Configuration=Release //p:Platform=x86")
     (projectile-project-compilation-cmd . "\"$(cygpath -u \"$(../../tools/vswhere.exe -latest -products \"*\" -requires Microsoft.Component.MSBuild -property installationPath)\")\"/MSBuild/15.0/Bin/MSBuild.exe IRIS.OpenLab.sln //v:Minimal //nologo //p:Configuration=Release //p:Platform=x86")
     (cider-lein-parameters . "with-profile +dirac repl :headless :host ::"))))
 '(sml/mode-width 0)
 '(sml/vc-mode-show-backend t))
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
 '(evil-search-highlight-persist-highlight-face ((t (:background "dark slate gray" :foreground "IndianRed1"))))
 '(powerline-active1 ((t (:background "#373844" :foreground "#f8f8f2"))))
 '(powerline-active2 ((t (:background "#373844" :foreground "#f8f8f2")))))
