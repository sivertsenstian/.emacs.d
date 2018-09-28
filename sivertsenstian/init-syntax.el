;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - SYNTAX MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package company
  :commands (company-mode global-company-mode company-complete
			  company-complete-common company-manual-begin company-grab-line)
 :straight t
 :config
 (setq company-idle-delay 0.5
       companu-minimum-prefix-length 3
       company-tooltip-limit 10
       company-dabbrev-downcase nil
       company-dabbrev-ignore-case nil
       company-dabbrev-code-other-buffers t
       company-tooltip-align-annotations t
       company-require-match 'never
       company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
       company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
       company-backends '(company-capf company-dabbrev company-ispell))
 
 (global-company-mode +1)) 

(use-package company-quickhelp
 :after company
 :straight t
 :config
 (setq company-quickhelp-delay nil)
 (company-quickhelp-mode +1))

;; (use-package company-prescient
;;   :after company
;;   :straight t
;;   :config
;;   (setq prescient-save-file "~/.emacs.d/prescient-save.el")
;;   (company-prescient-mode)
;;   (prescient-persist-mode +1))

(use-package company-box
  :after company
  :straight t
  :hook (company-mode . company-box-mode)
  :config
  (setq
   company-box-backends-colors nil
   company-box-max-candidates 50
   company-box-icons-yasnippet (all-the-icons-material "short_text" :height 0.8 :face 'all-the-icons-green)
   company-box-icons-unknown (all-the-icons-material "find_in_page" :height 0.8 :face 'all-the-icons-purple)
   company-box-icons-elisp
   (list (all-the-icons-material "functions"                        :height 0.8 :face 'all-the-icons-red)
         (all-the-icons-material "check_circle"                     :height 0.8 :face 'all-the-icons-blue)
         (all-the-icons-material "stars"                            :height 0.8 :face 'all-the-icons-orange)
         (all-the-icons-material "format_paint"                     :height 0.8 :face 'all-the-icons-pink))
   company-box-icons-lsp
   '((1  . (all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green)) ; text
     (2  . (all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; method
     (3  . (all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; function
     (4  . (all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; constructor
     (5  . (all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; field
     (6  . (all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))  ; variable
     (7  . (all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))   ; class
     (8  . (all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))   ; interface
     (9  . (all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))   ; module
     (10 . (all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))   ; property
     (11 . (all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))   ; unit
     (12 . (all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))   ; value
     (13 . (all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))   ; enum
     (14 . (all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))   ; keyword
     (15 . (all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))   ; snippet
     (16 . (all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))   ; color
     (17 . (all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))   ; file
     (18 . (all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))   ; reference
     (19 . (all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))   ; folder
     (20 . (all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))   ; enumMember
     (21 . (all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))   ; constant
     (22 . (all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))   ; struct
     (23 . (all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))   ; event
     (24 . (all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))   ; operator
     (25 . (all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))))

   (defun +company*box-frontend-even-if-single (command)
     (cond ((eq command 'hide)
            (company-box-hide))
           ((equal company-candidates-length 0)
            (company-box-hide))
           ((eq command 'update)
            (company-box-show))
           ((eq command 'post-command)
            (company-box--post-command))))

   (advice-add #'company-box-frontend :override #'+company*box-frontend-even-if-single))

(use-package flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :straight t
  :config
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package flycheck-pos-tip
  :after flycheck
  :straight t
  :config
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode))

(use-package browse-kill-ring
  :commands (browse-kill-ring
	     browse-kill-ring-forward
	     browse-kill-ring-previous
	     browse-kill-ring-search-forward
	     browse-kill-ring-search-backward
	     browse-kill-ring-prepend-insert
	     browse-kill-ring-append-insert)
  :straight t
  :config
  (setq browse-kill-ring-show-preview t
	browse-kill-ring-highlight-current-entry t
	browse-kill-ring-highlight-inserted-item 'pulse))

(use-package undo-tree
  :straight t
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

(use-package rainbow-delimiters
  :straight t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-indentation
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode)
  :straight t
  :config
  (set-face-background 'highlight-indentation-face "#195466")
  (set-face-background 'highlight-indentation-current-column-face "#26859E"))

(use-package lispy
 :straight t)

(use-package smartparens
 :straight t
 :init
 (add-hook 'prog-mode-hook #'smartparens-mode)
 (add-hook 'prog-mode-hook #'smartparens-strict-mode))

;; export
(provide 'init-syntax)
