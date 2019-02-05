;; -*- lexical-binding: t; -*-
;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - WEB MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------

(defun ng2--counterpart-name (file)
  "Return the file name of FILE's counterpart, or FILE if there is no counterpart."
  (when (not (ng2--is-component file)) file)
  (let ((ext (file-name-extension file))
        (base (file-name-sans-extension file)))
    (if (equal ext "ts")
        (concat base ".html")
      (concat base ".ts"))))

(defun ng2--sans-type (file)
  "Return the FILE's basename, sans its extensions."
  (file-name-sans-extension (file-name-sans-extension file)))

(defun ng2--is-component (file)
  "Return whether FILE is a component file."
  (equal (file-name-extension (file-name-sans-extension file)) "component"))

(defun ng2-open-counterpart ()
  "Opens the corresponding template or component file to this one."
  (interactive)
  (find-file (ng2--counterpart-name (buffer-file-name))))

(defun setup-tide-mode ()
  "Set up Tide mode."
  (interactive)
  (tide-setup)
  (tide-format-before-save)
  (tide-hl-identifier-mode nil)
  (eldoc-mode +1)
  (electric-pair-mode)
  (electric-quote-mode)
  (setq tide-completion-detailed t))

(use-package json-mode
  :mode "\\.json$"
  :straight t)

(use-package tide
  :after (:any web-mode typescript-mode html-mode)
  :straight t
  :config
  (setq company-tooltip-align-annotations t
	tide-completion-detailed t
        tide-always-show-documentation t)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (setq-default flycheck-disabled-checker 'javascript-jshint)
  (flycheck-add-next-checker
   'javascript-eslint
   'javascript-tide
   'append)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package add-node-modules-path
  :straight t)

(use-package prettier-js
  :after add-node-modules-path
  :straight t
  :hook (html-mode . add-node-modules-path) ;;use project prettier
  :hook (html-mode . prettier-js-mode)
  :hook (web-mode . add-node-modules-path) ;;use project prettier
  :hook (web-mode . prettier-js-mode)
  :hook (typescript-mode . add-node-modules-path) ;;use project prettier
  :hook (typescript-mode . prettier-js-mode)
  :hook (scss-mode . add-node-modules-path) ;;use project prettier
  :hook (scss-mode . prettier-js-mode)
  :hook (less-css-mode . add-node-modules-path) ;;use project prettier
  :hook (less-css-mode . prettier-js-mode)
  )

(use-package typescript-mode
  :mode ("\\.ts$" "\\.js$")
  :straight t
  :config
  (setq js2-basic-offset 4)
  (setq-default typescript-indent-level 4)

  (map! :map typescript-mode-map
	(:localleader
	  :n  "j" #'tide-jump-to-definition
	  :n  "J" #'tide-jump-to-implementation
	  :n  "b" #'tide-jump-back
	  :n  "d" #'tide-documentation-at-point
	  :n  "r" #'tide-rename-symbol
	  :n  "g" #'tide-nav
	  :n  "i" #'tide-organize-imports
	  :n  "u" #'tide-references
	  :n  "f" #'tide-fix
	  :n  "o" #'ng2-open-counterpart)))

(use-package emmet-mode
  :straight t
  :config
  (add-hook 'html-mode #'emmet-mode)
  (add-hook 'web-mode #'emmet-mode)
  (add-hook 'less-css-mode #'emmet-mode))

;; export
(provide 'init-web) 
