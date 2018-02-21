;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - EVIL MODULE INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package evil-leader
 :straight t
 :init
 (setq evil-leader/in-all-states t)
 (global-evil-leader-mode)
 :config
 (evil-leader/set-leader "<SPC>"))

(use-package evil
 :after evil-leader
 :straight t
 :init
 (evil-mode 1))

(use-package evil-magit
 :after (evil magit)
 :straight t)

(use-package evil-surround
  :after evil
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-exchange
  :after evil
  :straight t)

(use-package evil-commentary
  :after evil
  :straight t
  :config
  (evil-commentary-mode))

(use-package evil-snipe
  :after evil
  :straight t
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-search-highlight-persist
:after evil
:straight t
:config
(global-evil-search-highlight-persist t))

(use-package evil-escape
  :after evil
  :straight t
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.2))
;; export
(provide 'init-evil)
