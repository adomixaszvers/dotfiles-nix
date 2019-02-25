(require 'package)

;; optional. makes unpure packages archives unavailable
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)
(setq-default display-line-numbers-type 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)
(setq display-line-numbers 'relative)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs-saves/"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups
(setq default-frame-alist '((font . "FuraMono Nerd Font Mono-9")))

(defalias 'yes-or-no-p 'y-or-n-p)
(require 'use-package)
(use-package ivy
  :init
  (setq ivy-display-style 'fancy)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :config
  (ivy-mode 1))
(use-package counsel
  :config
  (counsel-mode 1))
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))
(use-package dashboard
  :init
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))
(use-package flycheck
  :config
  (global-flycheck-mode))
(use-package swiper
  :bind
  (("C-s" . swiper)))
(use-package magit
  :config
  (global-magit-file-mode))
(use-package which-key
  :config
  (which-key-mode))
