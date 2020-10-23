;; -*- lexical-binding: t; -*-
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
  :after (:all csharp-mode company)
  :hook (csharp-mode . omnisharp-mode)
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
  (eldoc-mode)

  (setq compile-command "dotnet build")

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  (setq projectile-compile-cmd (concat "dotnet build " projectile-project-root))
  (setq projectile-run-cmd (concat "dotnet run " projectile-project-root))

  (map! :map omnisharp-mode-map
        (:localleader
          :n "B" (λ! (compile (concat "dotnet build " projectile-project-root)))
          :n "R" (λ! (compile (concat "dotnet run " projectile-project-root)))
          :n "j" #'omnisharp-go-to-definition
          :n "J" #'omnisharp-go-to-definition-other-window

          (:desc "refactor" :prefix "r"
            :n "i"  #'omnisharp-fix-code-issue-at-point
            :n "u"  #'omnisharp-fix-usings
            :n "r"  #'omnisharp-rename
            :n "a"  #'omnisharp-show-last-auto-complete-result
            :n "o"  #'omnisharp-show-overloads-at-point)

          (:desc "find" :prefix "f"
            :n "u"  #'omnisharp-helm-find-usages
            :n "s"  #'omnisharp-helm-find-symbols
            :n "i"  #'omnisharp-find-implementations)

          (:desc "navigate" :prefix "n"
            :n "f"  #'omnisharp-navigate-to-current-file-member
            :n "m"  #'omnisharp-navigate-to-solution-member
            :n "M"  #'omnisharp-navigate-to-solution-file-then-file-member
            :n "F"  #'omnisharp-navigate-to-solution-file
            :n "r"  #'omnisharp-navigate-to-region)
          
          (:desc "type" :prefix "t"
            :n "i" #'omnisharp-current-type-information
            :n "d" #'omnisharp-current-type-documentation)

          (:desc "server" :prefix "s"
            :n "s" 'omnisharp-start-omnisharp-server
            :n "S" 'omnisharp-stop-server
            :n "r" 'omnisharp-reload-solution)

          (:desc "test" :prefix "t"
            :n "r" (λ! (omnisharp-unit-test "fixture"))
            :n "s" (λ! (omnisharp-unit-test "single"))
            :n "a" (λ! (omnisharp-unit-test "all"))))))

(use-package dotnet
  :straight t
  :after csharp-mode)

;; export
(provide 'init-csharp)
