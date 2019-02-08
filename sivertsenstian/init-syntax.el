;; -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - SYNTAX MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package company
  :commands (company-mode global-company-mode company-complete
			  company-complete-common company-manual-begin company-grab-line)
  :straight t
  :config
  (setq company-idle-delay 0.5
	company-minimum-prefix-length 3
	company-tooltip-limit 10
	company-dabbrev-downcase nil
	company-dabbrev-ignore-case nil
	company-dabbrev-code-other-buffers t
	company-tooltip-align-annotations t
	company-require-match 'never
	company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
	company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
	company-backends '(company-capf company-dabbrev company-ispell)
	company-transformers '(company-sort-by-occurrence))
  
  (global-company-mode +1)) 

(use-package company-quickhelp
  :after company
  :straight t
  :config
  (setq company-quickhelp-delay nil)
  (company-quickhelp-mode +1))

(use-package company-prescient
  :after company
  :straight t
  :config
  (setq prescient-save-file "~/.emacs.d/prescient-save.el")
  (company-prescient-mode)
  (prescient-persist-mode +1))

(use-package flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :straight t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package flycheck-pos-tip
  :after flycheck
  :straight t
  :config
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode))

(use-package browse-kill-ring
  :commands (browse-kill-ring
	     browse-kill-ring-forward
	     browse-kill-ring-previous
	     browse-kill-ring-search-forward
	     browse-kill-ring-search-backward
	     browse-kill-ring-prepend-insert
	     browse-kill-ring-append-insert)
  :straight t
  :config
  (setq browse-kill-ring-show-preview t
	browse-kill-ring-highlight-current-entry t
	browse-kill-ring-highlight-inserted-item 'pulse))

(use-package rainbow-delimiters
  :straight t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-indentation
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode)
  :straight t
  :config
  (set-face-background 'highlight-indentation-face "#195466")
  (set-face-background 'highlight-indentation-current-column-face "#26859E"))

(use-package lispy
  :defer t
  :straight t)

(use-package smartparens
  :straight t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (add-hook 'prog-mode-hook #'smartparens-strict-mode))

;; export
(provide 'init-syntax)
