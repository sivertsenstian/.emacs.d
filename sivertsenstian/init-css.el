;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - CSS MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package helm-css-scss
  :after web-mode
  :straight t)

(use-package rainbow-mode
  :after (:any css-mode scss-mode less-css-mode web-mode)
  :straight t
  :hook (css-mode sass-mode))

(use-package css-mode
  :straight t
  :mode "\\.css$"
  :mode ("\\.scss$" . scss-mode)
  :config
  (map! :map scss-mode-map
  (:localleader
    :nv "b" #'+css/scss-build)))

(use-package sass-mode
  :straight t
  :mode "\\.sass$"
  :config
  (map! :map scss-mode-map
  (:localleader
    :nv "b" #'+css/sass-build)))

(use-package less-css-mode
  :straight t
  :mode "\\.less$")

;; export
(provide 'init-css)
