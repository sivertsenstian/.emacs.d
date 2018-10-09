;; -*- lexical-binding: t; -*- ;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - HYDRA MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(use-package hydra
  :straight t
  :config
  (setq lv-use-seperator t)
  
  (defhydra hydra--text-zoom (:hint t :color red)
    "
      Text zoom: _j_:zoom in, _k_:zoom out, _0_:reset
"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("0" (text-scale-set 0) "reset" :color blue))

  (defhydra hydra--paste (:hint t :color red)
    "
      Paste from killring: _j_:next, _k_:previous, _l_:
"
    ("j" evil-paste-pop "next")
    ("k" evil-paste-pop-next "prev")
    ("p" evil-paste-after nil)
    ("P" evil-paste-before nil)
    ("l" helm-show-kill-ring "list" :color blue))

  (defhydra hydra--paste (:hint t :color red)
    "
      Paste from killring: _j_:next, _k_:previous, _l_:
"
    ("M-p" evil-paste nil)
    ("j" evil-paste-pop "next")
    ("k" evil-paste-pop-next "previous")
    ("l" helm-show-kill-ring "list")))

;; export
(provide 'init-hydra)
