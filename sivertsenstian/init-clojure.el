;; -*- lexical-binding: t; -*-
;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - CLOJURE MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(defun clojure/fancify-symbols (mode)
  "Pretty symbols for Clojure's anonymous functions and sets,
   like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
  (font-lock-add-keywords mode
			  `(("(\\(fn\\)[[[:space:]]"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "λ")
				       nil)))
			    ("(\\(partial\\)[[[:space:]]"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "ρ")
				       nil)))
			    ("(\\(comp\\)[[[:space:]]"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "∘")
				       nil)))
			    ("\\(#\\)("
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "ƒ")
				       nil)))
			    ("\\(#\\){"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "∈")
				       nil))))))


(defun spacemacs/clj-find-var ()
  "Attempts to jump-to-definition of the symbol-at-point. If CIDER fails, or not available, falls back to dumb-jump"
  (interactive)
  (let ((var (cider-symbol-at-point)))
    (if (and (cider-connected-p) (cider-var-info var))
        (unless (eq 'symbol (type-of (cider-find-var nil var)))
          (dumb-jump-go))
      (dumb-jump-go))))

;;;;
(use-package clojure-mode
  :straight t
  :mode "\\.clj$"
  :mode ("\\.cljc$" . clojurec-mode)
  :mode ("\\.cljs$" . clojurescript-mode)
  :config
  (setq cider-repl-history-file "~/.emacs.d/cider.history.log"
	clojure-align-forms-automatically t
	cider-repl-pop-to-buffer-on-connect nil
	cider-repl-use-clojure-font-lock t
	cider-repl-use-pretty-printing t
	cider-prompt-for-symbol nil
	cider-show-error-buffer nil
	cider-repl-display-help-banner nil
	clojure-align-forms-automatically t)

  (map! :map clojure-mode-map
	(:localleader
	  :nv "'" #'cider-jack-in
	  :nv "SPC" #'clojure-align
	  :nv "f" #'cider-format-buffer
	  (:desc "repl" :prefix "r"
	    :nv "c" #'cider-find-and-clear-repl-output)
	  (:desc "cider" :prefix "c"
	    :nv "e" #'cider-eval-last-sexp
	    :nv "f" #'cider-eval-defun-at-point
	    :n  "B" #'cider-switch-to-repl-buffer
	    :n  "b" #'cider-eval-buffer
	    :n  "n" #'cider-repl-set-ns
	    :n  "v" #'cider-find-var
	    :n  "j" #'cider-find-dwim
	    :n  "J" #'cider-find-dwim-other-window
	    :n  "d" #'cider-doc
	    :nv "m" #'cider-macroexpand-1
	    :n  "p" #'cider-eval-sexp-at-point
	    :n  "r" #'cider-eval-region)
	  (:desc "edit" :prefix "e"
	    :nv "a" #'clojure-align
	    :nv "k" #'clojure-toggle-keyword-string
	    :nv "l" #'clojure-move-to-let
	    :nv "L" #'clojure-introduce-let
	    :nv "i" #'clojure-cycle-if
	    :nv "w" #'clojure-cycle-when
	    :nv "P" #'clojure-cycle-privacy
	    :nv "n" #'clojure-cycle-not
	    :nv "}" #'clojure-convert-collection-to-map
	    :nv "]" #'clojure-convert-collection-to-vector
	    :nv "#" #'clojure-convert-collection-to-vector)
	  (:desc "go" :prefix "g"
	    :nv "i" #'clojure--goto-if
	    :nv "w" #'clojure--goto-when
	    :nv "l" #'clojure--goto-let)))

  (clojure/fancify-symbols 'clojure-mode)
  (clojure/fancify-symbols 'clojurescript-mode)
  (clojure/fancify-symbols 'clojurec-mode)
  (eldoc-mode)
  (subword-mode)
  (lispy-mode)

  (defun format-before-save-hook ()
    (when
	(or (eq major-mode 'clojure-mode)
	    (eq major-mode 'clojurescript-mode))
      (message "formatting buffer!")))
  (add-hook 'before-save-hook #'format-before-save-hook))


(use-package clj-refactor
  :straight t
  :after clojure-mode
  :config
  (clj-refactor-mode))

(use-package cider-eval-sexp-fu
  :straight t)

(use-package helm-cider
  :straight t
  :after cider
  :config
  (helm-cider-mode 1))

(use-package cider
  :straight t
  :after clojure-mode
  :config
  (setq cider-prompt-for-symbol nil)
  (setq cider-prefer-local-resources t)
  ;;(setq cider-font-lock-dynamically '(macro core function var))
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package cider-eval-sexp-fu
  :after clojure-mode
  :straight t)

(use-package flycheck-joker
  :after (clojure-mode flycheck)
  :straight t)

;; export
(provide 'init-clojure)
