; -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - HELM ++ MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package helm
  :straight t
  :defer t
  :init
  (setq helm-quick-update t
        ;; Display extraineous helm UI elements
        helm-display-header-line nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        ;; Don't override evil-ex's completion
        helm-mode-handle-completion-in-region nil
        helm-candidate-number-limit 20
        ;; Don't wrap item cycling
        helm-move-to-line-cycle-in-source t)
  (helm-mode 1)
  :config
  (setq helm-use-frame-when-more-than-two-windows nil
        helm-autoresize-max-height 20
        helm-autoresize-min-height 20
        helm-display-function #'display-buffer)
  (helm-autoresize-mode 1)
  (add-to-list 'display-buffer-alist
               '("\\`\\*helm"
                 (display-buffer-in-side-window)
                 (window-height . 0.2))))

(use-package helm-company
  :after (helm company)
  :straight t)

(use-package projectile
  :straight t
  :defer t
  :config
  (projectile-global-mode)
  (setq projectile-indexing-method 'hybrid
        projectile-enable-caching t
        projectile-completion-system 'helm))

(use-package helm-projectile
  :after (helm projectile)
  :straight t
  :config
  (helm-projectile-on))

(use-package pcre2el
  :commands rxt-quote-pcre
  :straight t)

(use-package helm-ag
  :after (helm projectile)
  :straight t
  :init
  (progn
    (defun sivertsenstian//helm-do-ag-region-or-symbol (func &optional dir)
      "Search with `ag' with a default input."
      (require 'helm-ag)
      (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
                 ;; make thing-at-point choosing the active region first
                 ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
                 ((symbol-function 'thing-at-point)
                  (lambda (thing)
                    (let ((res (if (region-active-p)
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end))
                                 (this-fn thing))))
                      (when res (rxt-quote-pcre res))))))
        (funcall func dir)))
    (defun sivertsenstian/helm-project-do-ag ()
      "Search in current project with `ag'."
      (interactive)
      (let ((dir (projectile-project-root)))
        (if dir
            (helm-do-ag dir)
          (message "error: Not in a project."))))

    (defun sivertsenstian/helm-project-do-ag-this-file ()
      "Search in current project with `ag'."
      (interactive)
      (sivertsenstian//helm-do-ag-region-or-symbol 'helm-do-ag-this-file dir))
    )
  :init
  (custom-set-variables
   '(helm-ag-base-command "rg --no-heading --vimgrep --smart-case")
   '(helm-ag-insert-at-point 'symbol)))

(use-package helm-swoop
  :commands (helm-swoop helm-multi-swoop helm-multi-swoop-all)
  :straight t
  :init
  (defun sivertsenstian/helm-swoop-region-or-symbol ()
    "Call `helm-swoop' with default input."
    (interactive)
    (let ((helm-swoop-pre-input-function
           (lambda ()
             (if (region-active-p)
                 (buffer-substring-no-properties (region-beginning)
                                                 (region-end))
               (let ((thing (thing-at-point 'symbol t)))
                 (if thing thing ""))))))
      (call-interactively 'helm-swoop)))
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-candidate-number-limit 200
        helm-swoop-speed-or-color t
        helm-swoop-pre-input-function (lambda () "")
        helm-swoop-split-with-multiple-windows t
        helm-swoop-split-direction 'split-window-vertically))

(use-package helm-themes
  :commands helm-themes
  :straight t)

(use-package deadgrep
  :straight t)

;; export
(provide 'init-helm)
