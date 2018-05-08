;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - ELM MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package elm-mode;
  :mode "\\.elm$"
  :straight t
  :config
  (add-to-list 'company-backends 'company-elm)
  (setq elm-sort-imports-on-save t)
  (setq elm-format-on-save t)
  (map! :map elm-mode-map
	(:localleader
	  :nv "=" #'elm-mode-format-buffer
	  :nv "b" #'elm-compile-buffer
	  :nv "B" #'elm-compile-main
	  :nv "d" #'elm-oracle-doc-at-point
	  :nv "D" #'elm-documentation-lookup
	  :nv "t" #'elm-oracle-type-at-point
	  :nv "f" #'elm-repl-push-decl
	  :nv "F" #'elm-repl-push
	  :nv "i" #'elm-sort-imports
	  :nv "I" #'elm-import
	  :nv "c" #'elm-package-catalog
	  :nv "p" #'elm-preview-buffer
	  :nv "P" #'elm-preview-main
	  :nv "'" #'elm-repl-load))
  (map! :map elm-package-mode-map
	:nvig "r" 'elm-package-refresh
        :nvig "v" 'elm-package-view
        :nvig "m" 'elm-package-mark
        :nvig "u" 'elm-package-unmark
        :nvig "x" 'elm-package-install
        :nvig "q" 'quit-window))

(use-package flycheck-elm
 :after (:all flycheck elm-mode)
 :hook (flycheck-mode . flycheck-elm-setup)
 :straight t)

;; export
(provide 'init-elm)
