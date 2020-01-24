;; -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - ARDUINO MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------

(use-package arduino-mode
  :mode ("\\.ino$" "\\.h$")
  :straight t
  :config
  (eldoc-mode +1)
  (electric-pair-mode)
  (electric-quote-mode))

(use-package irony
  :after arduino-mode
  :straight t)

(use-package company-irony
  :after irony
  :straight t)

(use-package company-c-headers
  :after irony
  :straight t)

(use-package company-arduino
  :after arduino-mode
  :straight t
  :config

  (add-hook 'irony-mode-hook 'company-arduino-turn-on)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-c-headers)

  (defun my-company-c-headers-get-system-path ()
    "Return the system include path for the current buffer."
    (let ((default '("/usr/include/" "/usr/local/include/")))
      (company-arduino-append-include-dirs default t)))
  (setq company-c-headers-path-system 'my-company-c-headers-get-system-path))

;; export
(provide 'init-arduino) 
