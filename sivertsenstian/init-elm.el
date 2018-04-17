;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - ELM MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package elm-mode;
  :mode "\\.elm$"
  :straight t
  :config
  (add-to-list 'company-backends 'company-elm)
  (setq elm-sort-imports-on-save t)
  (setq elm-format-on-save t))

(use-package flycheck-elm
 :after elm-mode
 :hook (flycheck-mode . flycheck-elm-setup)
 :straight t)

;; export
(provide 'init-elm)
