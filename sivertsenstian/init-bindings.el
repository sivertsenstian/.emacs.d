;; -*- lexical-binding: t; -*-
;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - BINDINGS AND KEY MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package which-key
  :straight t
  :defer t
  :config
  (setq which-key-sort-order 'which-key-prefix-then-key-order
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 5)
  (which-key-mode))

(defun simulate-key-press (key)
  "Pretend that KEY was pressed.
KEY must be given in `kbd' notation."
  `(lambda ()
     (interactive)
     (setq prefix-arg current-prefix-arg)
     (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key)))))

  ;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state);; slack

;; KEYBINDINGS
(map! [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag

      ;; Ensure there are no conflicts
      :nmvo doom-leader-key nil
      :nmvo doom-localleader-key nil

      ;; --- Global keybindings ---------------------------
      ;; Make M-x available everywhere
      :gnvime "M-x" #'helm-M-x
      :gnvime "A-x" #'helm-M-x

      ;; Text-scaling
      "M-="       (λ! (text-scale-set 0))
      "M-+"       #'text-scale-increase
      "M--"       #'text-scale-decrease

      ;; --- Indentation ----------------------------------
      :ne "M-q" #'prog-indent-sexp ;;indent-pp-sexp
      :ne "C-M-q" #'indent-pp-sexp

      ;; --- Kill ring ------------------------------------
      :ne "M-p" #'hydra--paste/evil-paste-after
      :ne "M-P" #'hydra--paste/evil-paste-before

      ;; --- Personal vim-esque bindings ------------------
      :n  "]b" #'next-buffer
      :n  "[b" #'previous-buffer
      :n  "]w" #'evil-window-next
      :n  "[h" #'git-gutter+-previous-hunk
      :n  "]h" #'git-gutter+-next-hunk
      :n  "[w" #'evil-window-prev
      :m  "gd" #'smart-jump-go
      :m  "gb" #'smart-jump-back
      :m  "gh" #'smart-jump-references

      :en "C-f"   #'helm-swoop
      :en "M-f"   #'helm-multi-swoop-projectile
      ;; Easier code navigation
      :en "C-M-k"   #'evil-backward-section-begin
      :en "C-M-j"   #'evil-forward-section-begin
      :en "C-k"   #'evil-backward-paragraph
      :en "C-j"   #'evil-forward-paragraph

      :en "C-h"   #'evil-first-non-blank
      :en "C-l"   #'evil-end-of-line
      :en ","     #'(simulate-key-press "SPC m")

      ;; --- <leader> -------------------------------------
      (:leader
	;; Most commonly used
	:desc "Find file in project"    :n "SPC" #'projectile-find-file
	:desc "Switch buffer"           :n "TAB" (λ! (switch-to-buffer (other-buffer (current-buffer) 1)))
	:desc "Browse files"            :n "."   #'dired
	:desc "Browse buffers"          :n ","   #'helm-mini
	:desc "Jump to mark"            :n "RET" #'evil-goto-mark
	:desc "search"                  :n "/"   #'isearch-forward
	:desc "search symbol"           :n "*"   #'sivertsenstian/helm-project-do-ag
	:desc "window"                  :n "w"   evil-window-map
	:desc "shell"                   :n "!"   #'powershell
	:desc "winum-select-window-0"   :n "0"   #'winum-select-window-0-or-10
	:desc "zoom"                    :n "z"   #'hydra--text-zoom/body 
	:n "1" #'winum-select-window-1
	:n "2" #'winum-select-window-2
	:n "3" #'winum-select-window-3
	:n "4" #'winum-select-window-4
	:n "5" #'winum-select-window-5
	:n "6" #'winum-select-window-6
	:n "7" #'winum-select-window-7
	:n "8" #'winum-select-window-8
	:n "9" #'winum-select-window-9

	(:desc "paredit"     :prefix "k"
	  :desc "Slurp sexp forward"   :nv "s" #'lispy-forward-slurp-sexp
	  :desc "Slurp sexp backward"  :nv "S" #'lispy-backward-slurp-sexp
	  :desc "Barf sexp forward"    :nv "b" #'lispy-forward-barf-sexp
	  :desc "Barf sexp backward"   :nv "B" #'lispy-backward-barf-sexp
	  :desc "Kill"                 :nv "x" #'lispy-kill
	  :desc "Kill!"                :nv "X" #'lispy-kill-at-point
	  :desc "toggle mode"          :nv "m" #'lispy-mode
	  :desc "wrap sexp"            :nv "w" #'lispy-wrap-round
	  :desc "unwrap sexp"          :nv "W" #'lispy-splice
	  :desc "wrap sexp in []"      :nv "]" #'lispy-wrap-brackets
	  :desc "wrap sexp in {}"      :nv "}" #'lispy-wrap-braces
	  :desc "stringify"            :nv "\"" #'lispy-stringify
	  :desc "unstringify"          :nv "'" #'lispy-unstringify
	  :desc "toggle strict"        :nv "!" #'smartparens-strict-mode
	  :desc "Indent sexp"          :nv "TAB" #'prog-indent-sexp)

	(:desc "search" :prefix "s"
	  :desc "search"                :nv "s" #'helm-ag
	  :desc "resume"                :nv "r" #'helm-resume
	  :desc "in project"            :nv "p" #'sivertsenstian/helm-project-do-ag
	  :desc "in project (results)"  :nv "P" #'sivertsenstian/helm-project-do-ag-region-or-symbol
	  :desc "Swoop"                 :nv "f" #'sivertsenstian/helm-swoop-region-or-symbol
	  :desc "Swoop project"         :nv "F" #'helm-multi-swoop-projectile
	  :desc "Imenu"                 :nv "j" #'helm-imenu
	  :desc "Imenu across buffers"  :nv "J" #'helm-imenu-anywhere
	  :desc "Browse kill ring"      :nv "k" #'helm-show-kill-ring)

	(:desc "error" :prefix "e"
	  :desc "next"                  :nv "j" #'flycheck-next-error
	  :desc "previous"              :nv "k" #'flycheck-previous-error)

	(:desc "buffer" :prefix "b"
	  :desc "New empty buffer"        :n "n" #'evil-buffer-new
	  :desc "Kill buffer"             :n "k" #'kill-this-buffer
	  :desc "Save buffer"             :n "s" #'save-buffer
	  :desc "Save buffers"            :n "S" #'save-some-buffers
	  :desc "Revert buffer"           :n "r" #'revert-buffer
	  :desc "Evaluate buffer"         :n "E" #'eval-buffer
	  :desc "Next buffer"             :n "]" #'next-buffer
	  :desc "Previous buffer"         :n "[" #'previous-buffer)

	(:desc "code" :prefix "c"
	  (:desc "edit" :prefix "e"
	    :desc "Edit symbol (in buffer)"  :n  "s" #'iedit-mode
	    :desc "Edit symbol (in fn)"      :n  "S" #'iedit-mode-toggle-on-function
	    :desc "Restrict"                 :n  "r" #'iedit-restrict-function
	    :desc "Upcase"                   :n  "u" #'iedit-upcase-occurrences
	    :desc "Downcase"                 :n  "d" #'iedit-downcase-occurrences
	    :desc "Delete"                   :n  "!" #'iedit-delete-occurrences
	    :desc "Case sensitive"           :n  "c" #'iedit-toggle-case-sensitive
	    :desc "Quit"                       :n  "q" #'iedit-quit)
	  :desc "List errors"                :n  "x" #'flycheck-list-errors
	  :desc "Jump to definition"         :n  "j" #'smart-jump-go
	  :desc "Jumpt to definition (dumb)" :n  "J" #'dumb-jump-go
	  :desc "Jump to references"         :n  "r" #'smart-jump-find-references-with-rg)

	(:desc "file" :prefix "f"
	  :desc "Find file"                 :n "f" #'helm-find-files
	  :desc "Save file"                 :n "s" #'save-buffer
	  :desc "Save files"                :n "S" #'save-some-buffers
	  :desc "Find file in project"      :n "p" #'helm-projectile-find-file
	  :desc "Find other file"           :n "a" #'helm-projectile-find-other-file
	  :desc "Recent files"              :n "R" #'helm-recentf
	  :desc "Recent project files"      :n "r" #'helm-projectile-recentf)

	(:desc "git" :prefix "g"
	  :desc "Git status"            :n  "s" #'magit-status
	  :desc "Git blame"             :n  "b" #'magit-blame
	  :desc "Git time machine"      :n  "t" #'git-timemachine-toggle
	  :desc "Git stage hunk"        :n  "S" #'git-gutter+-stage-hunks
	  :desc "Git revert hunk"       :n  "r" #'git-gutter+-revert-hunk
	  :desc "Git revert buffer"     :n  "R" #'vc-revert
	  :desc "Git show hunk inline"  :n  "i" #'git-gutter+-show-hunk-inline-at-point
	  :desc "Git show hunk"         :n  "I" #'git-gutter+-show-hunk
	  :desc "Next hunk"             :nv "]" #'git-gutter+-next-hunk
	  :desc "Previous hunk"         :nv "[" #'git-gutter+-previous-hunk)

	(:desc "help" :prefix "h"
	  :n "h" help-map
	  (:desc "define" :prefix "d"
	    :desc "Describe function"     :n  "f" #'describe-function
	    :desc "Describe key"          :n  "k" #'describe-key
	    :desc "Describe variable"     :n  "v" #'describe-variable
	    :desc "Describe at point"     :n  "." #'helpful-at-point
	    :desc "Find definition"       :n  "d" #'+lookup/definition
	    :desc "Find references"       :n  "r" #'+lookup/references
	    :desc "Find documentation"    :n  "h" #'+lookup/documentation)
	  :desc "Highlight symbol"      :nv "h" #'highlight-symbol-at-point
	  :desc "Unhighlight search"    :nv "s" #'evil-search-highlight-persist-remove-all
	  :desc "Unhighlight symbol"    :nv "u" #'unhighlight-regexp)

	(:desc "jump" :prefix "j"
	  :desc "to char"               :nv "j" #'avy-goto-char
	  :desc "to word"               :nv "w" #'avy-goto-word-1
	  :desc "to line above"         :nv "K" #'avy-goto-line-above
	  :desc "to line below"         :nv "J" #'avy-goto-line-below
	  :desc "Imenu"                 :nv "s" #'helm-imenu
	  :desc "Imenu across buffers"  :nv "S" #'helm-imenu-anywhere
	  :desc "to definition"         :nv "d" #'evil-goto-definition)

	(:desc "project" :prefix "p"
	  :desc "Browse project"          :n  "." #'+default/browse-project
	  :desc "Find file in project"    :n  "f" #'helm-projectile-find-file
	  :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
	  :desc "Switch project"          :n  "p" #'projectile-switch-project
	  :desc "Recent project files"    :n  "r" #'projectile-recentf
	  :desc "search in project"           :nv "s" #'sivertsenstian/helm-project-do-ag
	  :desc "search in project (results)" :nv "S" #'sivertsenstian/helm-project-do-ag-region-or-symbol
	  :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)

	(:desc "quit" :prefix "q"
	  :desc "Reload"                 :n "r" #'sivertsenstian/reload-init
	  :desc "Quit"                   :n "q" #'evil-save-and-quit)

	(:desc "toggle" :prefix "t"
	  :desc "Flycheck"               :n "f" #'flycheck-mode
	  :desc "Truncate lines"         :n "t" #'toggle-truncate-lines
	  :desc "Frame fullscreen"       :n "F" #'toggle-frame-fullscreen
	  :desc "New frame"              :n "N" #'make-frame
	  :desc "Indent guides"          :n "i" #'highlight-indentation-mode
	  :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
	  :desc "Theme"                  :n "T" #'helm-themes))


      ;; --- Personal vim-esque bindings ------------------
      :nv "gj" #'evil-avy-goto-char
      :nv "gw" #'evil-avy-goto-word-1
      :nv "gJ" #'evil-avy-goto-line-below
      :nv "gK" #'evil-avy-goto-line-above
      :v  "."  #'evil-repeat
      :nv "C-a" #'org-increase-number-at-point
      :nv "C-M-a" #'org-decrease-number-at-point

      ;; --- Plugin bindings ------------------------------
      ;; company-mode (vim-like omnicompletion)
      :i "TAB"    #'+company/complete
      :i "C-SPC"  #'+company/complete
      (:after company
	(:map company-active-map
	  ;; Don't interfere with `evil-delete-backward-word' in insert mode
	  "C-w"        nil
	  "C-o"        #'company-search-kill-others
	  "C-j"        #'company-select-next
	  "C-k"        #'company-select-previous
	  "]"          #'company-select-next
	  "["          #'company-select-previous
	  "C-h"        #'company-quickhelp-manual-begin
	  "C-S-h"      #'company-show-doc-buffer
	  "C-S-s"      #'company-search-candidates
	  "C-s"        #'company-filter-candidates
	  "C-SPC"      #'company-complete-common
	  "C-h"        #'company-quickhelp-manual-begin
	  [tab]        #'company-complete-common-or-cycle
	  [backtab]    #'company-select-previous
	  [escape]     (λ! (company-abort) (evil-normal-state 1)))
	;; Automatically applies to `company-filter-map'
	(:map company-search-map
	  "C-j"        #'company-search-repeat-forward
	  "C-k"        #'company-search-repeat-backward
	  "C-s"        (λ! (company-search-abort) (company-filter-candidates))
	  [escape]     #'company-search-abort))

      ;;helm
      (:after helm
	(:map helm-map
	  "C-j" #'helm-next-line
	  "C-k" #'helm-previous-line))
      ;; evil
      (:after evil
	(:map evil-window-map		; prefix "C-w"
	  ;; Navigation
	  "C-h"     #'evil-window-left
	  "C-j"     #'evil-window-down
	  "C-k"     #'evil-window-up
	  "C-l"     #'evil-window-right
	  "w"       #'ace-window
	  "_"	    #'ace-delete-other-windows
	  "s"       #'ace-swap-window
	  "d"       #'ace-delete-window
	  "u"       #'winner-undo
	  "r"       #'winner-redo))

      ;; evil-commentary
      :n  "gc"  #'evil-commentary
      :n  "gy"  #'evil-commentary-yank
      ;; evil-exchange
      :n  "gx"  #'evil-exchange

      ;; evil-matchit
      :nv [tab]   #'indent-for-tab-command
      :nv [S-tab] #'evil-jump-item
      :nv "S"     #'browse-kill-ring
      ;; evil-magit
      (:after evil-magit
	:map (magit-status-mode-map magit-revision-mode-map)
	:n [tab] #'magit-section-toggle
	:n "C-j" nil
	:n "C-k" nil)

      ;; evil-multiedit
      :v  "R"     #'evil-multiedit-match-all
      :n  "M-d"   #'evil-multiedit-match-symbol-and-next
      :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
      :v  "M-d"   #'evil-multiedit-match-and-next
      :v  "M-D"   #'evil-multiedit-match-and-prev
      :nv "C-M-d" #'evil-multiedit-restore
      (:after evil-multiedit
	(:map evil-multiedit-state-map
	  "M-d" #'evil-multiedit-match-and-next
	  "M-D" #'evil-multiedit-match-and-prev
	  "RET" #'evil-multiedit-toggle-or-restrict-region)
	(:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
	  "C-j" #'evil-multiedit-next
	  "C-k" #'evil-multiedit-prev))

      ;; evil-snipe
      (:after evil-snipe
	:map evil-snipe-parent-transient-map
	;; switch to evil-easymotion/avy after a snipe
	"C-;" (λ! (require 'evil-easymotion)
		  (call-interactively
		   (evilem-create #'evil-snipe-repeat
				  :bind ((evil-snipe-scope 'whole-buffer)
					 (evil-snipe-enable-highlight)
					 (evil-snipe-enable-incremental-highlight))))))

      ;; evil-surround
      :v  "S"  #'evil-surround-region
      :o  "s"  #'evil-surround-edit
      :o  "S"  #'evil-Surround-edit

      ;; flycheck
      :m  "]e" #'flycheck-next-error
      :m  "[e" #'flycheck-previous-error
      (:after flycheck
	:map flycheck-error-list-mode-map
	:n "C-j" #'flycheck-error-list-next-error
	:n "C-k" #'flycheck-error-list-previous-error
	:n "j"   #'flycheck-error-list-next-error
	:n "k"   #'flycheck-error-list-previous-error
	:n "RET" #'flycheck-error-list-goto-error)

      ;; git-timemachine
      (:after git-timemachine
	(:map git-timemachine-mode-map
	  :n "C-k" #'git-timemachine-show-previous-revision
	  :n "C-j" #'git-timemachine-show-next-revision
	  :n "[["  #'git-timemachine-show-previous-revision
	  :n "]]"  #'git-timemachine-show-next-revision
	  :n "q"   #'git-timemachine-quit
	  :n "gb"  #'git-timemachine-blame))

      ;; hl-todo
      :m  "]t" #'hl-todo-next
      :m  "[t" #'hl-todo-previous
      (:after browse-kill-ring
	:map browse-kill-ring-mode-map
	"j" #'browse-kill-ring-forward
	"k" #'browse-kill-ring-previous
	"J" #'browse-kill-ring-search-forward
	"K" #'browse-kill-ring-search-backward
	"P" #'browse-kill-ring-prepend-insert
	"p" #'browse-kill-ring-append-insert)

      ;; undo-tree -- undo/redo for visual regions
      :v "C-u" #'undo-tree-undo
      :v "C-r" #'undo-tree-redo

      ;; --- Built-in plugins -----------------------------
      (:after comint
	;; TAB auto-completion in term buffers
	:map comint-mode-map [tab] #'company-complete)

      (:after debug
	;; For elisp debugging
	:map debugger-mode-map
	:n "RET" #'debug-help-follow
	:n "e"   #'debugger-eval-expression
	:n "n"   #'debugger-step-through
	:n "c"   #'debugger-continue)

      (:map help-mode-map
	:n "[["  #'help-go-back
	:n "]]"  #'help-go-forward
	:n "o"   #'ace-link-help
	:n "q"   #'quit-window)

      (:after vc-annotate
	:map vc-annotate-mode-map
	:n "q"   #'kill-this-buffer
	:n "d"   #'vc-annotate-show-diff-revision-at-line
	:n "D"   #'vc-annotate-show-changeset-diff-revision-at-line
	:n "SPC" #'vc-annotate-show-log-revision-at-line
	:n "]]"  #'vc-annotate-next-revision
	:n "[["  #'vc-annotate-prev-revision
	:n "TAB" #'vc-annotate-toggle-annotation-visibility
	:n "RET" #'vc-annotate-find-revision-at-line))

;;
;; Keybinding fixes
;;

;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim, or how I like it.

(map! (:map input-decode-map
	[S-iso-lefttab] [backtab]
	(:unless window-system "TAB" [tab])) ; Fix TAB in terminal
      ;; textmate-esque newline insertion
      :ni [M-return]     #'evil-open-below
      :ni [S-M-return]   #'evil-open-above
      ;; textmate-esque deletion
      :i [backspace]    #'delete-backward-char
      ;; Emacsien motions for insert mode
      :i "C-b" #'backward-word
      :i "C-f" #'forward-word

      ;; Restore common editing keys (and ESC) in minibuffer
      (:map (minibuffer-local-map
	     minibuffer-local-ns-map
	     minibuffer-local-completion-map
	     minibuffer-local-must-match-map
	     minibuffer-local-isearch-map
	     read-expression-map)
	[escape] #'abort-recursive-edit
	"C-r" #'evil-paste-from-register
	"C-a" #'move-beginning-of-line
	"C-b" #'backward-word
	"C-f" #'forward-word)

      (:after evil
	(:map evil-ex-completion-map
	  "C-a" #'move-beginning-of-line))

      (:map messages-buffer-mode-map
	"M-;" #'eval-expression
	"A-;" #'eval-expression)

      (:after tabulated-list
	(:map tabulated-list-mode-map
	  [remap evil-record-macro] #'quit-window))

      (:after view
	(:map view-mode-map "<escape>" #'View-quit-all)))

;; export
(provide 'init-bindings)
