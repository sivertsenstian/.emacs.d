(use-package company-shell
  :straight t)


(use-package insert-shebang)
(use-package sh-script)

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; export
(provide 'init-shell)
