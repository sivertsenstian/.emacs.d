;; -*- lexical-binding: t; -*-
;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - PYTHON MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------

(use-package python
  :mode "\\.py"
  :straight t
  :config

  (add-to-list 'company-backends #'company-anaconda)

  (company-mode)
  (flycheck-mode)
  (eldoc-mode)
  (anaconda-mode)
  (anaconda-eldoc-mode)
  )

(use-package anaconda-mode
  :after python
  :straight t)

(use-package company-anaconda
  :after (:all company anaconda-mode)
  :straight t)

(use-package lsp-pyright
 :after (:all flycheck anaconda-mode)
 :straight t)

(use-package pipenv
  :after anaconda-mode
  :straight t)

(use-package pyimport
  :after anaconda-mode
  :straight t)

(use-package py-isort
  :after anaconda-mode
  :straight t)

(use-package blacken
  :straight t
  :config
  (blacken-mode))

;; export
(provide 'init-python)
