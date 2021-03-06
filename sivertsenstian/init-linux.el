;; -*- lexical-binding: t; -*-
;----------------------------------------------------------------------------
;; SIVERTSENSTIAN - LINUX SPECIFIC MODULES INIT AND CONFIGURATION
;;----------------------------------------------------------------------------
(set-frame-font "Hack  10" nil t) 

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-init)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project))

(use-package helm-spotify-plus
  :after helm
  :straight t
  :init
  (map! (:leader
	  (:desc "music"          :prefix "u"
	    :desc "play/pause "   :n "SPC" #'helm-spotify-plus-toggle-play-pause
	    :desc "next"          :n "j" #'helm-spotify-plus-next
	    :desc "previous"      :n "k" #'helm-spotify-plus-previous
	    :desc "search"        :n "s" #'helm-spotify-plus)))
  :commands (helm-spotify-plus
	     helm-spotify-plus-next
	     helm-spotify-plus-previous
	     helm-spotify-plus-toggle-play-pause))


(provide 'init-linux)
