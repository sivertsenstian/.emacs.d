;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - CSS MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(map! :map* (css-mode-map scss-mode-map less-css-mode-map)
      :n "M-R" #'+css/web-refresh-browser
      (:localleader
        :n  "rb" #'+css/toggle-inline-or-block))

;; (use-package counsel-css
;;   :straight t
;;   :commands (counsel-css counsel-css-imenu-setup)
;;   :hook (css-mode . counsel-css-imenu-setup)
;;   :init
;;   (map! :map* (css-mode-map scss-mode-map less-css-mode-map)
;;         :localleader :n ";" #'counsel-css))

(use-package rainbow-mode
  :straight t
  :hook (css-mode sass-mode))

(use-package css-mode
  :straight t
  :mode "\\.css$"
  :mode ("\\.scss$" . scss-mode)
  :config
  (set! :company-backend '(css-mode scss-mode) '(company-css company-yasnippet))
  (map! :map scss-mode-map :localleader "b" #'+css/scss-build))


(use-package sass-mode
  :straight t
  :mode "\\.sass$"
  :config
  (set! :company-backend 'sass-mode '(company-css company-yasnippet))
  (map! :map scss-mode-map :localleader "b" #'+css/sass-build))

(use-package less-css-mode
  :straight t
  :mode "\\.less$")

(use-package stylus-mode
  :straight t
  :mode "\\.styl$"
  :init (add-hook! stylus-mode #'(yas-minor-mode-on flycheck-mode)))

;; export
(provide 'init-css)
