;; -*- lexical-binding: t; -*-
;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - RUST MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package rust-mode;
  :mode "\\.rs$"
  :straight t
  :config
  (setq rust-format-on-save t)
  (eldoc-mode +1)
  (company-mode)
  (map! :map rust-mode-map
	(:localleader
	  :nv "=" #'rust-format-buffer
	  :nv "d" #'racer-describe-tooltip
	  :nv "D" #'racer-describe
	  :nv "j" #'racer-find-definition
	  :nv "J" #'racer-find-definition-other-window
	  (:desc "cargo" :prefix "c"
	    :nv "b" #'cargo-process-build
	    :nv "r" #'cargo-process-run
	    :nv "c" #'cargo-process-clean
	    ))))

(use-package racer
  :after rust-mode
  :hook (rust-mode . racer-mode)
  :straight t)


(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode)
  :straight t)

(use-package flycheck-rust
 :after (:all flycheck rust-mode)
 :hook (flycheck-mode . flycheck-rust-setup)
 :straight t)

;; export
(provide 'init-rust)
