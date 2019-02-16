(setq inhibit-startup-message t) ;; no startup msg
(setq evil-want-integration nil) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq-default display-line-numbers-type 'visual
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers 'relative)
(require 'evil)
(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "f" 'counsel-projectile-find-file
  "cp" 'counsel-projectile)
(global-evil-leader-mode)
(when (require 'evil-collection nil t)
  (evil-collection-init))
(evil-mode 1)
