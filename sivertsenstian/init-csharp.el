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
  :after company 
  :hook (csharp-mode . omnisharp-mode)
  ;;:commands omnisharp-install-server
  :preface
  (setq omnisharp-auto-complete-want-documentation nil
        omnisharp-cache-directory  "~/.emacs.d/omnisharp")
  :config
  (let ((omnisharp-bin (or omnisharp-server-executable-path (omnisharp--server-installation-path t))))
    (unless (file-exists-p omnisharp-bin)
      (warn! "Omnisharp server isn't installed, completion won't work")))

  (add-to-list 'company-backends #'company-omnisharp)
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq compile-command "dotnet build")

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile)

  (map! :map omnisharp-mode-map
	:localleader
        :n "b" #'omnisharp-recompile

        (:prefix "r"
          :n "i"  #'omnisharp-fix-code-issue-at-point
          :n "u"  #'omnisharp-fix-usings
          :n "r"  #'omnisharp-rename
          :n "a"  #'omnisharp-show-last-auto-complete-result
          :n "o"  #'omnisharp-show-overloads-at-point)

        (:prefix "f"
          :n "u"  #'omnisharp-find-usages
          :n "i"  #'omnisharp-find-implementations
          :n "f"  #'omnisharp-navigate-to-current-file-member
          :n "m"  #'omnisharp-navigate-to-solution-member
          :n "M"  #'omnisharp-navigate-to-solution-file-then-file-member
          :n "F"  #'omnisharp-navigate-to-solution-file
          :n "r"  #'omnisharp-navigate-to-region
          :n "ti" #'omnisharp-current-type-information
          :n "td" #'omnisharp-current-type-documentation)

        (:prefix "t"
          :n "r" (λ! (omnisharp-unit-test "fixture"))
          :n "s" (λ! (omnisharp-unit-test "single"))
          :n "a" (λ! (omnisharp-unit-test "all")))))

;; export
(provide 'init-csharp)
