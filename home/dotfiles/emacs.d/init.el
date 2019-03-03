;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
(when (file-readable-p "~/.emacs.d/config.el")
  (load-file (expand-file-name "~/.emacs.d/config.el")))
