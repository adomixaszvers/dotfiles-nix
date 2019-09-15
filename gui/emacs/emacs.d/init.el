(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(require 'package)
;; optional. makes unpure packages archives unavailable
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook #'benchmark-init/deactivate)

;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
(when (file-readable-p "~/.emacs.d/config.el")
  (load-file (expand-file-name "~/.emacs.d/config.el")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-readable-p custom-file)
  (load custom-file))