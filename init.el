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

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(use-package init-evil)
(use-package init-themes)
(use-package init-syntax)
(use-package init-bindings)
(use-package init-ivy)
(use-package init-files)
(use-package init-navigation)
(use-package init-git)
(use-package init-clojure)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bd19fec77fa640b63cbf22f8328222b3dc4c8f891f63be6ea861187bc6c6c4e4" "d8cec8251169ccfe192aa87d69b9378bc81599330f31498f85deaef633721302" "b72213f8339991fbd58f9ac518a16c30fd2986c0e28415788f19ed3014ce3141" "a94bd69f1140c41fd22174d6d082de915c96124c54cea7ddd5d41e166ead905b" "0c5a64b9705db7b8cf871d84ce6a90553468f313190d52cb3cfcdf7b1a0ceec1" default)))
 '(package-selected-packages (quote (evil)))
 '(safe-local-variable-values
   (quote
    ((projectile-project-compile-cmd . "\"$(cygpath -u \"$(../../tools/vswhere.exe -latest -products \"*\" -requires Microsoft.Component.MSBuild -property installationPath)\")\"/MSBuild/15.0/Bin/MSBuild.exe IRIS.OpenLab.sln //v:Minimal //nologo //p:Configuration=Release //p:Platform=x86")
     (projectile-project-compilation-cmd . "\"$(cygpath -u \"$(../../tools/vswhere.exe -latest -products \"*\" -requires Microsoft.Component.MSBuild -property installationPath)\")\"/MSBuild/15.0/Bin/MSBuild.exe IRIS.OpenLab.sln //v:Minimal //nologo //p:Configuration=Release //p:Platform=x86")
     (cider-lein-parameters . "with-profile +dirac repl :headless :host ::"))))
 '(sml/theme (quote respectful)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
