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

(defun ng2-open-counterpart-other-window ()
  "Opens the corresponding template or component file to this one in the other window."
  (interactive)
  (find-file-other-window (ng2--counterpart-name (buffer-file-name))))

(defun setup-tide-mode ()
  "Set up Tide mode."
  (interactive)
  (tide-setup)
  ;; (tide-format-before-save)
  (tide-hl-identifier-mode nil)
  (eldoc-mode +1)
  (electric-pair-mode)
  (electric-quote-mode)
  (setq tide-completion-detailed t))

(use-package json-mode
  :mode "\\.json$"
  :straight t)

(use-package tide
  :after (:any web-mode typescript-mode)
  :straight t
  :config
  (setq company-tooltip-align-annotations t
        tide-completion-detailed t
        tide-always-show-documentation t)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'web-mode-hook #'setup-tide-mode)

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
	  :desc "jump to definition"      :n  "j" #'tide-jump-to-definition
	  :desc "jump to implementation " :n  "J" #'tide-jump-to-implementation
	  :desc "jump back"               :n  "b" #'tide-jump-back
	  :desc "documentation at point"  :n  "d" #'tide-documentation-at-point
	  :desc "rename symbol"           :n  "r" #'tide-rename-symbol
	  :desc "navigate to"             :n  "g" #'tide-nav
	  :desc "organize imports"        :n  "i" #'tide-organize-imports
	  :desc "references"              :n  "u" #'tide-references
	  :desc "fix"                     :n  "f" #'tide-fix
	  :desc "angular comp. template"  :n  "o" #'ng2-open-counterpart
	  :desc "angular comp. template other" :n  "O" #'ng2-open-counterpart-other-window

	  ;; :desc "typescript jack-in"    :nv #'run-ts
	  (:desc "repl" :prefix "c"
	    :n "e" #'ts-send-last-sexp
	    :n "E" #'ts-send-last-sexp-and-go
	    :n "b" #'ts-send-last-buffer
	    :n "B" #'ts-send-last-buffer-and-go
	    :n "l" #'ts-load-file-and-go)
	  )))
	  

(use-package ts-comint
  :after typescript-mode
  :straight t)

(use-package emmet-mode
  :straight t
  :config
  (setq emmet-expand-jsx-className? t) 

  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'typescript-mode-hook 'emmet-mode))

(use-package company-web
  :straight t)

(use-package web-mode
  :straight t
  :after (company-web company)
  :mode ("\\.html$" "\\.tsx$" "\\.jsx$")
  :config

  ;; Tag auto-close style:
  ;; 0=no auto-closing
  ;; 1=auto-close with </
  ;; 2=auto-close with > and </.
  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 1)

  (setq js2-basic-offset 4) 
  (setq-default typescript-indent-level 4)

  (add-to-list 'company-backends #'company-web-html)

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)

  (map! :map web-mode-map
	(:localleader
	  :desc "jump to definition"      :n  "j" #'tide-jump-to-definition
	  :desc "jump to implementation " :n  "J" #'tide-jump-to-implementation
	  :desc "jump back"               :n  "b" #'tide-jump-back
	  :desc "documentation at point"  :n  "d" #'tide-documentation-at-point
	  :desc "rename symbol"           :n  "r" #'tide-rename-symbol
	  :desc "navigate to"             :n  "g" #'tide-nav
	  :desc "organize imports"        :n  "i" #'tide-organize-imports
	  :desc "references"              :n  "u" #'tide-references
	  :desc "fix"                     :n  "f" #'tide-fix
	  :desc "angular comp. template"  :n  "o" #'ng2-open-counterpart
	  :desc "angular comp. template other" :n  "O" #'ng2-open-counterpart-other-window

	  ;; :desc "typescript jack-in"    :nv #'run-ts
	  (:desc "repl" :prefix "c"
	    :n "e" #'ts-send-last-sexp
	    :n "E" #'ts-send-last-sexp-and-go
	    :n "b" #'ts-send-last-buffer
	    :n "B" #'ts-send-last-buffer-and-go
	    :n "l" #'ts-load-file-and-go)
	  )))

;; export
(provide 'init-web) 
