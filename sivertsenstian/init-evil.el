;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - EVIL MODULE INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package evil
 :after evil-leader
 :straight t
 :init
 (evil-mode 1)
 (add-to-list 'evil-insert-state-modes 'shell-mode)
 (add-to-list 'evil-insert-state-modes 'dashboard-mode)
 (add-to-list 'evil-insert-state-modes 'git-timemachine-mode))

(use-package evil-leader
 :straight t
 :init
 (setq evil-leader/in-all-states t)
 (global-evil-leader-mode)
 :config
 (evil-leader/set-leader "<SPC>"))

(use-package evil-escape
  :after evil
  :straight t
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  :config
  (push #'minibufferp evil-escape-inhibit-functions)
  (map! :irvo "C-g" #'evil-escape)
  (evil-escape-mode))

(use-package evil-exchange
  :commands evil-exchange
  :straight t)

(use-package evil-matchit
  :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :straight t
  :config (global-evil-matchit-mode 1)
  :init
  (map! [remap evil-jump-item] #'evilmi-jump-items
        :textobj "%" #'evilmi-text-object #'evilmi-text-object))

(use-package evil-snipe
  :commands (evil-snipe-mode evil-snipe-override-mode
			     evil-snipe-local-mode evil-snipe-override-local-mode)
  :straight t
  :init
  (add-hook 'prog-mode-hook 'evil-snipe-mode)
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t
        evil-snipe-disabled-modes '(magit-mode elfeed-show-mode elfeed-search-mode)
        evil-snipe-aliases '((?\[ "[[{(]")
                             (?\] "[]})]")
                             (?\; "[;:]")))
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-args
  :commands (evil-inner-arg evil-outer-arg
			    evil-forward-arg evil-backward-arg
			    evil-jump-out-args)
  :straight t)

(use-package evil-magit
 :after (evil magit)
 :straight t)

(use-package evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :straight t
  :config
  (evil-commentary-mode 1))

(use-package evil-search-highlight-persist
 :after evil
 :straight t
 :config
 (global-evil-search-highlight-persist t))

;; (use-package evil-goggles
;;   :after evil
;;   :straight t
;;   :config
;;   (setq evil-goggles-blocking-duration 0.100)
;;   (evil-goggles-mode)
;;   (evil-goggles-use-diff-faces))

(use-package evil-multiedit
  :after evil
  :straight t
  :config
  (evil-multiedit-default-keybinds))

;; export
(provide 'init-evil)
