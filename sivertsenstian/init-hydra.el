;----------------------------------------------------------------------------
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
    ("j" (yank-pop 1) "next")
    ("k" (yank-pop -1) "previous")
    ("l" helm-show-kill-ring "list"))

  (defhydra hydra--move (:hint t :color red)
    "
      Move: _h_:beginning, _l_:end, _j_:down, _k_:up, _m_: mark
"
    ("h" evil-first-non-blank "beginning" :color blue)
    ("l" evil-end-of-line "end" :color blue)
    ("j" evil-scroll-down "down")
    ("k" evil-scroll-up "up")
    ("m" evil-goto-mark "mark" :color blue)))

;; export
(provide 'init-hydra)
