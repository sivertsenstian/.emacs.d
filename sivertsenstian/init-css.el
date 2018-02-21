;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - CSS MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package counsel-css
  :straight (counsel-css :type git :host github :repo "hlissner/emacs-counsel-css")
  :commands (counsel-css counsel-css-imenu-setup)
  :hook (css-mode . counsel-css-imenu-setup)
  :init
  (map! :map* (css-mode-map scss-mode-map less-css-mode-map)
        :localleader :n ";" #'counsel-css))

(use-package rainbow-mode
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
