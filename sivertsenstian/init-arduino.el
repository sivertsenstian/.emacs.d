;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - ARDUINO MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package arduino-mode
 :straight t
 :mode "\\.ino$"
 :config
 (map! :map arduino-mode-map
  (:localleader)))

(use-package irony
  :straight t
  :mode "\\.cpp$")

(use-package company-irony
  :after irony
  :straight t
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :after irony
  :straight t)

(use-package platformio-mode
  :after arduino-mode
  :straight t
  :config
  (add-hook 'c++-mode-hook (lambda ()
			     (irony-mode)
			     (irony-eldoc)
			     (platformio-conditionally-enable)))
  (add-hook 'irony-mode-hook
	    (lambda ()
	      (define-key irony-mode-map [remap completion-at-point]
		'irony-completion-at-point-async)

	      (define-key irony-mode-map [remap complete-symbol]
		'irony-completion-at-point-async)

	      (irony-cdb-autosetup-compile-options)))

  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

;; export
(provide 'init-arduino)
