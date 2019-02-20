(setq-default display-line-numbers-type 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers 'relative)
(eval-when-compile
    (package-initialize)
    (require 'use-package))
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
    (setq evil-want-integration nil) ;; This is optional since it's already set to t by default.
    (setq evil-want-C-u-scroll t)
    :config
    (evil-mode 1))
(use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init))
