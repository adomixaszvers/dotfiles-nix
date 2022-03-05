;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 12.0))
(setq doom-unicode-font doom-font)
(setq! display-line-numbers-type 'visual)
(when (display-graphic-p)
  (setq doom-theme 'doom-nord))
(setq! +notmuch-mail-folder "~/Maildir/adomixaszvers")
;;; Show all outlines while diffing org-mode files
(after! (ediff org)
  (add-hook 'ediff-prepare-buffer-hook
            (lambda () (when (eq major-mode 'org-mode) (outline-show-all)))))
(after! direnv (add-hook 'before-hack-local-variables-hook #'direnv-update-environment))
(after! nix-mode
  (set-company-backend! 'nix-mode nil))
;; (setq lsp-haskell-process-path-hie "hie")
(after! haskell
  (setq! lsp-haskell-formatting-provider "ormolu")
  (setq! lsp-haskell-importlens-on nil))

(setq +rss-elfeed-files `(,(expand-file-name "elfeed.org" doom-private-dir)))
(add-hook 'dired-mode-hook
          (lambda ()
            (when (file-remote-p dired-directory)
              (setq-local dired-actual-switches "-alhB"))))

(defun my-ediff-init ()
  "Diffs init.el with init.example.el."
  (interactive)
  (let ((my-init (expand-file-name "init.el" "~/.config/nixpkgs/profiles/gui/doom"))
        (upstream-init (expand-file-name "init.example.el" doom-emacs-dir)))
    (ediff-files my-init upstream-init)))

;; see https://github.com/hlissner/doom-emacs/issues/5904#issuecomment-997045485
(after! lsp-mode
  (advice-remove #'lsp #'+lsp-dont-prompt-to-install-servers-maybe-a))

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
