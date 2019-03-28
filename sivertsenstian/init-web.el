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
  :after (:any web-mode typescript-mode)
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
  (add-hook 'web-mode #'emmet-mode)
  (add-hook 'less-css-mode #'emmet-mode))

(use-package company-web
  :straight t)

(use-package web-mode
  :straight t
  :after (company-web company)
  :mode ("\\.html$")
  :config
  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 2)

  (add-to-list 'company-backends #'company-web-html)
  (map! :map web-mode-map
        (:localleader
          :desc "Rehighlight buffer"          :n "h" #'web-mode-buffer-highlight
          :desc "Indent buffer"               :n "i" #'web-mode-buffer-indent
	  :desc "Open component"              :n "o" #'ng2-open-counterpart
	  :desc "Open component other window" :n "O" #'ng2-open-counterpart-other-window

          (:desc "attributes" :prefix "a"
            "b" #'web-mode-attribute-beginning
            "e" #'web-mode-attribute-end
            "i" #'web-mode-attribute-insert
            "n" #'web-mode-attribute-next
            "s" #'web-mode-attribute-select
            "k" #'web-mode-attribute-kill
            "p" #'web-mode-attribute-previous
            "p" #'web-mode-attribute-transpose)

          (:desc "block" :prefix "b"
            "b" #'web-mode-block-beginning
            "c" #'web-mode-block-close
            "e" #'web-mode-block-end
            "k" #'web-mode-block-kill
            "n" #'web-mode-block-next
            "p" #'web-mode-block-previous
            "s" #'web-mode-block-select)

          (:desc "dom" :prefix "d"
            "a" #'web-mode-dom-apostrophes-replace
            "d" #'web-mode-dom-errors-show
            "e" #'web-mode-dom-entities-encode
            "n" #'web-mode-dom-normalize
            "q" #'web-mode-dom-quotes-replace
            "t" #'web-mode-dom-traverse
            "x" #'web-mode-dom-xpath)

          (:desc "element" :prefix "e"
            "/" #'web-mode-element-close
            "a" #'web-mode-element-content-select
            "b" #'web-mode-element-beginning
            "c" #'web-mode-element-clone
            "d" #'web-mode-element-child
            "e" #'web-mode-element-end
            "f" #'web-mode-element-children-fold-or-unfold
            "i" #'web-mode-element-insert
            "k" #'web-mode-element-kill
            "m" #'web-mode-element-mute-blanks
            "n" #'web-mode-element-next
            "p" #'web-mode-element-previous
            "r" #'web-mode-element-rename
            "s" #'web-mode-element-select
            "t" #'web-mode-element-transpose
            "u" #'web-mode-element-parent
            "v" #'web-mode-element-vanish
            "w" #'web-mode-element-wrap)

          (:desc "tag" :prefix "t"
	    "a" #'web-mode-tag-attributes-sort
	    "b" #'web-mode-tag-beginning
	    "e" #'web-mode-tag-end
	    "m" #'web-mode-tag-match
	    "n" #'web-mode-tag-next
	    "p" #'web-mode-tag-previous
	    "s" #'web-mode-tag-select))

        :g  "M-/" #'web-mode-comment-or-uncomment
        :i  "SPC" #'self-insert-command
        :n  "za"  #'web-mode-fold-or-unfold
        :nv "]a"  #'web-mode-attribute-next
        :nv "[a"  #'web-mode-attribute-previous
        :nv "]t"  #'web-mode-tag-next
        :nv "[t"  #'web-mode-tag-previous
        :nv "]T"  #'web-mode-element-child
        :nv "[T"  #'web-mode-element-parent))

;; export
(provide 'init-web) 
