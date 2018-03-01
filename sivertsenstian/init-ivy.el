;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - IVY, COUNSEL, AND SWIPER MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package ivy
 :straight t
 :init
 (ivy-mode 1)
 :config
 (setq ivy-use-virtual-buffers t
       ivy-virtual-abbreviate 'fullpath
       projectile-completion-system 'ivy
       ivy-count-format "%d/%d "
       enable-recursive-minibuffers t))

(use-package counsel
 :after ivy
 :straight t)

(use-package swiper
 :after ivy
 :straight t)

(use-package projectile
 :straight t
 :config
 (setq projectile-indexing-method 'native)
 (setq projectile-enable-caching t))

(use-package counsel-projectile
 :after (counsel projectile)
 :straight t)

;; export
(provide 'init-ivy)
