;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - NAVIGATION MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------

(use-package avy
 :straight t)

(use-package smart-jump
 :straight t
 :config
 (smart-jump-setup-default-registers)
 (smart-jump-register :modes '(clojure-mode)))

(use-package dumb-jump
 :straight t)

(use-package ace-window
 :straight t)

(use-package popwin
 :straight t
 :config
 (popwin-mode 1)
 (push '(ag-mode :stick t) popwin:special-display-config)
 (push "*Kill Ring*" popwin:special-display-config))

;; (use-package winum
;;  :straight t
;;  :init
;;  (setq winum-keymap
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "SPC 0") 'winum-select-window-0-or-10)
;;       (define-key map (kbd "SPC 1") 'winum-select-window-1)
;;       (define-key map (kbd "SPC 2") 'winum-select-window-2)
;;       (define-key map (kbd "SPC 3") 'winum-select-window-3)
;;       (define-key map (kbd "SPC 4") 'winum-select-window-4)
;;       (define-key map (kbd "SPC 5") 'winum-select-window-5)
;;       (define-key map (kbd "SPC 6") 'winum-select-window-6)
;;       (define-key map (kbd "SPC 8") 'winum-select-window-7)
;;       (define-key map (kbd "SPC 9") 'winum-select-window-8)
;;       map))
;;  :config
;;  (winum-mode))


;; export
(provide 'init-navigation)
