;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - EVIL MODULE INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package evil-leader
 :straight t
 :init
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

(use-package evil-escape
  :after evil
  :straight t
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.2))
;; export
(provide 'init-evil)
