;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - BINDINGS AND KEY MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package which-key
  :straight t
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

(define-key evil-motion-state-map (kbd "ø") (simulate-key-press "["))
(define-key evil-motion-state-map (kbd "æ") (simulate-key-press "]"))
(define-key evil-motion-state-map (kbd "Ø") (simulate-key-press "{"))
(define-key evil-motion-state-map (kbd "Æ") (simulate-key-press "}"))

(define-key evil-insert-state-map (kbd "ø") (simulate-key-press "["))
(define-key evil-insert-state-map (kbd "æ") (simulate-key-press "]"))
(define-key evil-insert-state-map (kbd "Ø") (simulate-key-press "{"))
(define-key evil-insert-state-map (kbd "Æ") (simulate-key-press "}"))

;; KEYBINDINGS
(map! [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag

      ;; Ensure there are no conflicts
      :nmvo doom-leader-key nil
      :nmvo doom-localleader-key nil

      ;; --- Global keybindings ---------------------------
      ;; Make M-x available everywhere
      :gnvime "M-x" #'execute-extended-command
      :gnvime "A-x" #'execute-extended-command

      ;; Text-scaling
      "M-="       (λ! (text-scale-set 0))
      "M-+"       #'text-scale-increase
      "M--"       #'text-scale-decrease

      ;; --- Indentation ----------------------------------
      :ne "M-q" #'prog-indent-sexp ;;indent-pp-sexp
      :ne "C-M-q" #'indent-pp-sexp

      ;; --- Kill ring ------------------------------------
      :ne "M-p" #'browse-kill-ring

      ;; --- Personal vim-esque bindings ------------------
      :n  "]b" #'next-buffer
      :n  "[b" #'previous-buffer
      :n  "]w" #'evil-window-next
      :n  "[w" #'evil-window-prev
      :m  "gd" #'+lookup/definition
      :m  "gD" #'+lookup/references
      :m  "gh" #'+lookup/documentation
      :n  "gp" #'+evil/reselect-paste
      :n  "gr" #'+eval:region
      :n  "gR" #'+eval/buffer
      :v  "gR" #'+eval:replace-region
      :v  "@"  #'+evil:macro-on-all-lines
      :n  "g@" #'+evil:macro-on-all-lines

      :en "C-f"   #'swiper
      ;; Easier window navigation
      :en "C-h"   #'evil-window-left
      :en "C-j"   #'evil-window-down
      :en "C-k"   #'evil-window-up
      :en "C-l"   #'evil-window-right

      ;; --- <leader> -------------------------------------
      (:leader
	;; Most commonly used
	:desc "Find file in project"    :n "SPC" #'projectile-find-file
	:desc "Switch buffer"           :n "TAB" (λ! (switch-to-buffer (other-buffer (current-buffer) 1)))
	:desc "Ex command"              :nv ";"  #'execute-extended-command
	:desc "M-x"                     :nv ":"  #'evil-ex
	:desc "Browse files"            :n "."   #'find-file
	:desc "Eval expression"         :n ","   #'switch-to-buffer
	:desc "Blink cursor line"       :n "DEL" (λ! (nav-flash-show))
	:desc "Jump to mark"            :n "RET" #'evil-goto-mark
	:desc "search"                  :n "/"   #'counsel-projectile-ag
	:desc "search symbol"           :n "*"   #'counsel-ag-thing-at-point
	:desc "window"                  :n "w"  evil-window-map
	:desc "winum-select-window-0"   :n "0" #'winum-select-window-0-or-10
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
	  :desc "Kill!"                :nv "X" #'lispy-kill-at-poin
	  :desc "toggle mode"          :nv "m" #'lispy-mode
	  :desc "wrap sexp"            :nv "w" #'lispy-wrap-round
	  :desc "unwrap sexp"          :nv "W" #'lispy-splice
	  :desc "Raise sexp"           :nv "r" #'lispy-raise-sexp
	  :desc "Convolute sexp"       :nv "r" #'lispy-convolute-sexp
	  :desc "wrap sexp in []"      :nv "[" #'lispy-wrap-brackets
	  :desc "wrap sexp in {}"      :nv "{" #'lispy-wrap-braces
	  :desc "stringify"            :nv "\"" #'lispy-stringify
	  :desc "unstringify"          :nv "'" #'lispy-stringify
	  :desc "toggle strict"        :nv "!" #'smartparens-strict-mode
	  :desc "Indent sexp"          :nv "TAB" #'prog-indent-sexp)

	(:desc "previous..." :prefix "["
	  :desc "Text size"             :nv "[" #'text-scale-decrease
	  :desc "Buffer"                :nv "b" #'previous-buffer
	  :desc "Diff Hunk"             :nv "d" #'git-gutter:previous-hunk
	  :desc "Todo"                  :nv "t" #'hl-todo-previous
	  :desc "Error"                 :nv "e" #'previous-error
	  :desc "Window"                :nv "w" #'evil-window-prev
	  :desc "Smart jump"            :nv "h" #'smart-backward)

	(:desc "next..." :prefix "]"
	  :desc "Text size"             :nv "]" #'text-scale-increase
	  :desc "Buffer"                :nv "b" #'next-buffer
	  :desc "Diff Hunk"             :nv "d" #'git-gutter:next-hunk
	  :desc "Todo"                  :nv "t" #'hl-todo-next
	  :desc "Error"                 :nv "e" #'next-error
	  :desc "Window"                :nv "w" #'evil-window-next
	  :desc "Smart jump"            :nv "l" #'smart-forward)

	(:desc "search" :prefix "s"
	  :desc "ag"                    :nv "a" #'ag
	  :desc "counsel find"          :nv "c" #'counsel-ag
	  :desc "in project"            :nv "p" #'projectile-ag
	  :desc "Swiper"                :nv "s" #'swiper
	  :desc "Avy goto"              :nv "j" #'evil-avy-goto-char
	  :desc "Imenu"                 :nv "j" #'imenu
	  :desc "Imenu across buffers"  :nv "J" #'imenu-anywhere
	  :desc "Browse kill ring"      :nv "k" #'browse-kill-ring)

	(:desc "error" :prefix "e"
	  :desc "next"                  :nv "n" #'next-error
	  :desc "previous"              :nv "p" #'previous-error
	  :desc "next"                  :nv "]" #'next-error
	  :desc "previous"              :nv "[" #'previous-error)

	(:desc "buffer" :prefix "b"
	  :desc "New empty buffer"        :n "n" #'evil-buffer-new
	  :desc "Switch buffer"           :n "b" #'switch-to-buffer
	  :desc "Kill buffer"             :n "k" #'kill-this-buffer
	  :desc "Save buffer"             :n "s" #'save-buffer
	  :desc "Save buffers"            :n "S" #'save-some-buffers
	  :desc "Bury buffer"             :n "z" #'bury-buffer
	  :desc "Next buffer"             :n "]" #'next-buffer
	  :desc "Previous buffer"         :n "[" #'previous-buffer)

	(:desc "code" :prefix "c"
		(:desc "edit" :prefix "e"
	   :desc "Edit symbol (in buffer)"  :n  "s" #'iedit-mode
		 :desc "Edit symbol (in fn)"      :n  "S" #'iedit-mode-toggle-on-function
     :desc "Restrict"                 :n  "r" #'iedit-restrict-function
     :desc "Upcase"                   :n  "u" #'iedit-upcase-occurences
     :desc "Downcase"                 :n  "d" #'iedit-downcase-occurences
		 :desc "Delete"                   :n  "!" #'iedit-delete-occurences
		 :desc "Case sensitive"           :n  "c" #'iedit-toggle-case-sensitive
		 :desc "Quit"                     :n  "q" #'iedit-quit)
	  :desc "List errors"                :n  "x" #'flycheck-list-errors
	  :desc "Jump to definition"         :n  "j" #'smart-jump-go
		:desc "Jumpt to definition (dumb)" :n  "J" #'dumb-jump-go
	  :desc "Jump to references"         :n  "r" #'smart-jump-find-references-with-ag)

	(:desc "file" :prefix "f"
	  :desc "Find file"                 :n "f" #'counsel-find-file
	  :desc "Save file"                 :n "s" #'save-buffer
	  :desc "Save files"                :n "S" #'save-some-buffers
	  :desc "Find file in project"      :n "p" #'projectile-find-file
	  :desc "Find file from here"       :n "j" #'counsel-file-jump
	  :desc "Find other file"           :n "a" #'projectile-find-other-file
	  :desc "Recent files"              :n "r" #'counsel-recentf
	  :desc "Recent project files"      :n "R" #'projectile-recentf)

	(:desc "git" :prefix "g"
	  :desc "Git status"            :n  "s" #'magit-status
	  :desc "Git blame"             :n  "b" #'magit-blame
	  :desc "Git time machine"      :n  "t" #'git-timemachine-toggle
	  :desc "Git stage hunk"        :n  "S" #'git-gutter:stage-hunk
	  :desc "Git revert hunk"       :n  "r" #'git-gutter:revert-hunk
	  :desc "Git revert buffer"     :n  "R" #'vc-revert
	  :desc "Next hunk"             :nv "]" #'git-gutter:next-hunk
	  :desc "Previous hunk"         :nv "[" #'git-gutter:previous-hunk)

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

	(:desc "Jump" :prefix "j"
	  :desc "to char"               :nv "j" #'avy-goto-char
	  :desc "to fn"                 :nv "f" #'imenu
	  :desc "to fn across buffers"  :nv "F" #'imenu-anywhere
	  :desc "to definition"         :nv "d" #'evil-goto-definition)

	(:desc "project" :prefix "p"
	  :desc "Browse project"          :n  "." #'+default/browse-project
	  :desc "Find file in project"    :n  "f" #'projectile-find-file
	  :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
	  :desc "Switch project"          :n  "p" #'projectile-switch-project
	  :desc "Recent project files"    :n  "r" #'projectile-recentf
	  :desc "List project tasks"      :n  "t" #'+ivy/tasks
	  :desc "ag in project"           :nv "a" #'projectile-ag
	  :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache
	  :desc "neotree"                 :nv "t" #'neotree-projectile-action)

	(:desc "quit" :prefix "q"
	  :desc "Quit"                   :n "q" #'evil-save-and-quit
	  :desc "Quit (forget session)"  :n "Q" #'+workspace/kill-session-and-quit)

	(:desc "toggle" :prefix "t"
	  :desc "Flycheck"               :n "f" #'flycheck-mode
	  :desc "Truncate lines"         :n "t" #'toggle-truncate-lines
	  :desc "Frame fullscreen"       :n "F" #'toggle-frame-fullscreen
	  :desc "Indent guides"          :n "i" #'highlight-indentation-mode
	  :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
	  :desc "Theme"                  :n "T" #'counsel-load-theme
	  :desc "Golden ratio"           :n "g" #'golden-ratio-mod))


      ;; --- Personal vim-esque bindings ------------------
      :n  "]b" #'next-buffer
      :n  "[b" #'previous-buffer
      :m  "gd" #'+lookup/definition
      :m  "gD" #'+lookup/references
      :m  "gh" #'+lookup/documentation
      :n  "gp" #'+evil/reselect-paste
      :n  "gr" #'+eval:region
      :n  "gR" #'+eval/buffer
      :v  "gR" #'+eval:replace-region
      :nv "gj" #'evil-avy-goto-char
      :nv "gJ" #'evil-avy-goto-line-below
      :nv "gK" #'evil-avy-goto-line-above
      ;; repeat in visual mode (FIXME buggy)
      :v  "."  #'evil-repeat
      ;; don't leave visual mode after shifting
      :v  "<"  #'+evil/visual-dedent  ; vnoremap < <gv
      :v  ">"  #'+evil/visual-indent  ; vnoremap > >gv
      ;; paste from recent yank register (which isn't overwritten)
      :v  "C-p" "\"0p"

      :nv "C-a" #'evil-numbers/inc-at-pt
      :nv "C-x" #'evil-numbers/dec-at-pt


      ;; --- Plugin bindings ------------------------------
      ;; company-mode (vim-like omnicompletion)
      :i "TAB"    #'+company/complete
      :i "C-SPC"  #'+company/complete
      (:after company
	(:map company-active-map
	  ;; Don't interfere with `evil-delete-backward-word' in insert mode
	  "C-w"        nil
	  "C-o"        #'company-search-kill-others
	  "C-n"        #'company-select-next
	  "C-p"        #'company-select-previous
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
	  "C-n"        #'company-search-repeat-forward
	  "C-p"        #'company-search-repeat-backward
	  "C-s"        (λ! (company-search-abort) (company-filter-candidates))
	  [escape]     #'company-search-abort))

      ;; counsel
      (:after counsel
	(:map counsel-ag-map
	  [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
	  "C-SPC"    #'ivy-call-and-recenter ; preview
	  ))

      ;; evil
      (:after evil
	(:map evil-window-map ; prefix "C-w"
	  ;; Navigation
	  "C-h"     #'evil-window-left
	  "C-j"     #'evil-window-down
	  "C-k"     #'evil-window-up
	  "C-l"     #'evil-window-right
	  "w"       #'ace-window
	  "_"				#'ace-delete-other-windows
	  "s"       #'ace-swap-window
	  ;; Delete window
	  "d"     #'ace-delete-window))

      ;; evil-commentary
      :n  "gc"  #'evil-commentary
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
	  "C-n" #'evil-multiedit-next
	  "C-p" #'evil-multiedit-prev))

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

      ;; expand-region
      :v  "v"  #'er/expand-region
      :v  "V"  #'er/contract-region

      ;; flycheck
      :m  "]e" #'next-error
      :m  "[e" #'previous-error
      (:after flycheck
	:map flycheck-error-list-mode-map
	:n "C-n" #'flycheck-error-list-next-error
	:n "C-p" #'flycheck-error-list-previous-error
	:n "j"   #'flycheck-error-list-next-error
	:n "k"   #'flycheck-error-list-previous-error
	:n "RET" #'flycheck-error-list-goto-error)

      ;; git-gutter
      :m  "]d" #'git-gutter:next-hunk
      :m  "[d" #'git-gutter:previous-hunk

      ;; git-timemachine
      (:after git-timemachine
	(:map git-timemachine-mode-map
	  :n "C-p" #'git-timemachine-show-previous-revision
	  :n "C-n" #'git-timemachine-show-next-revision
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
      ;; ivy
      (:after ivy
	:map ivy-minibuffer-map
	[escape] #'keyboard-escape-quit
	"C-SPC" #'ivy-call-and-recenter
	"M-v" #'yank
	"M-z" #'undo
	"C-r" #'evil-paste-from-register
	"C-k" #'ivy-previous-line
	"C-j" #'ivy-next-line
	"C-l" #'ivy-alt-done
	"C-w" #'ivy-backward-kill-word
	"C-u" #'ivy-kill-line
	"C-b" #'backward-word
	"C-f" #'forward-word)

      ;; neotree
      (:after neotree
	:map neotree-mode-map
	:n "g"         nil
	:n [tab]       #'neotree-quick-look
	:n "RET"       #'neotree-enter
	:n [backspace] #'evil-window-prev
	:n "c"         #'neotree-create-node
	:n "r"         #'neotree-rename-node
	:n "d"         #'neotree-delete-node
	:n "j"         #'neotree-next-line
	:n "k"         #'neotree-previous-line
	:n "n"         #'neotree-next-line
	:n "p"         #'neotree-previous-line
	:n "h"         #'+neotree/collapse-or-up
	:n "l"         #'+neotree/expand-or-open
	:n "J"         #'neotree-select-next-sibling-node
	:n "K"         #'neotree-select-previous-sibling-node
	:n "H"         #'neotree-select-up-node
	:n "L"         #'neotree-select-down-node
	:n "G"         #'evil-goto-line
	:n "gg"        #'evil-goto-first-line
	:n "v"         #'neotree-enter-vertical-split
	:n "s"         #'neotree-enter-horizontal-split
	:n "q"         #'neotree-hide
	:n "R"         #'neotree-refresh)

      ;; swiper
      (:after swiper
	(:map swiper-map
	  [backtab]  #'+ivy/wgrep-occur))

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
	:n "q"   #'quit-window
	:n "Q"   #'ivy-resume)

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
