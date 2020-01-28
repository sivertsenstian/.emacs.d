;; -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - FILE MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(defun sivertsenstian/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

;;dired
(setq ;; Always copy/delete recursively
 dired-recursive-copies  'always
 dired-recursive-deletes 'top
 ;; Auto refresh dired, but be quiet about it
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil)

(use-package dired-k
  :after dired
  :straight t
  :config
  (map! :map dired-mode-map
	:n "c" #'find-file
	:n "d" #'dired-do-delete
	:n "r" #'dired-do-rename)
  (setq dired-k-style 'git)
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

(use-package treemacs
  :straight t)

(use-package treemacs-evil
  :straight t)

(use-package treemacs-projectile
  :straight t)

;; export
(provide 'init-files)
