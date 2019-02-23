(require 'package)

;; optional. makes unpure packages archives unavailable
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)
(setq-default display-line-numbers-type 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs-saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups
(set-frame-font "FuraMono Nerd Font Mono-9")
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers 'relative)
(require 'use-package)
(use-package evil-leader
    :ensure t
    :after evil
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
    "f" 'counsel-projectile-find-file
    "cp" 'counsel-projectile
    "n" 'flycheck-next-error
    "p" 'flycheck-previous-error
    "ll" 'flycheck-list-errors)
    (global-evil-leader-mode))
(use-package evil
    :ensure t
    :init
    (setq evil-want-keybinding nil)
    (setq evil-want-integration nil)
    (setq evil-want-C-u-scroll t)
    :config
    (evil-mode 1))
(use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init))
