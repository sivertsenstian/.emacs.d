;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - C# MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
;; https://github.com/hlissner/doom-emacs/tree/master/modules/lang/csharp
;; https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/csharp/packages.el

(use-package csharp-mode
  :mode "\\.cs$"
  :straight t)

(use-package omnisharp
  :straight t
  :config
  (add-hook 'csharp-mode-hook #'(omnisharp-mode)))

;; (use-package omnisharp
;;   :after csharp-mode
;;   :straight t
;;   :hook (csharp-mode . #'(eldoc-mode flycheck-mode omnisharp-mode))
;;   :config
;;   (setq indent-tabs-mode nil)
;;   (setq c-syntactic-indentation t)
;;   (c-set-style "ellemtel")
;;   (setq c-basic-offset 4)
;;   (setq truncate-lines t)
;;   (setq tab-width 4)
;;   (setq evil-shift-width 4)
;;   (electric-pair-local-mode 1)

;;   (add-to-list 'company-backend 'company-omnisharp)
;;   (map! :map omnisharp-mode-map
;;         :m "gd" #'omnisharp-go-to-definition

;;         (:localleader
;; 	  :n "b" #'omnisharp-recompile

;; 	  (:prefix "r"
;; 	    :n "i"  #'omnisharp-fix-code-issue-at-point
;; 	    :n "u"  #'omnisharp-fix-usings
;; 	    :n "r"  #'omnisharp-rename
;; 	    :n "a"  #'omnisharp-show-last-auto-complete-result
;; 	    :n "o"  #'omnisharp-show-overloads-at-point)

;; 	  (:prefix "f"
;; 	    :n "u"  #'omnisharp-find-usages
;; 	    :n "i"  #'omnisharp-find-implementations
;; 	    :n "f"  #'omnisharp-navigate-to-current-file-member
;; 	    :n "m"  #'omnisharp-navigate-to-solution-member
;; 	    :n "M"  #'omnisharp-navigate-to-solution-file-then-file-member
;; 	    :n "F"  #'omnisharp-navigate-to-solution-file
;; 	    :n "r"  #'omnisharp-navigate-to-region
;; 	    :n "ti" #'omnisharp-current-type-information
;; 	    :n "td" #'omnisharp-current-type-documentation))))

;; export
(provide 'init-csharp)
