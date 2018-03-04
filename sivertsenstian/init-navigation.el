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
 ;(push '(ag-mode :stick t) popwin:special-display-config)
 (push '(rg-mode :stick t) popwin:special-display-config)
 (push "*Kill Ring*" popwin:special-display-config))

(use-package winum
 :straight t
 :init
 (push '(("\\(.*\\) 0" . "winum-select-window-0") . ("\\1 0..9" . "window 0..9"))
      which-key-replacement-alist)
 (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)
 :config
 (winum-mode))

;;  (use-package golden-ratio
;;   :straight t
;;   :config
;;   (golden-ratio-mode 1))

(use-package imenu-anywhere
 :straight t)
;; export
(provide 'init-navigation)
