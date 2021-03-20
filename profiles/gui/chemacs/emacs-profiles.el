;;; .emacs-profiles.el --- description -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs profiles configuration for Chemacs

;;; Code:
(("doom" . ((user-emacs-directory . "~/fast/doom-emacs/")
            (env . (("DOOMDIR" . "~/.config/nixpkgs/profiles/gui/doom/")))))
 ("prelude" . ((user-emacs-directory . "~/.emacs-prelude.d")))
 ("nix-doom" . ((user-emacs-directory . "~/.emacs-nix.d")))
 ("spacemacs" . ((user-emacs-directory . "~/.emacs-spacemacs.d"))))

(provide '.emacs-profiles)
;;; .emacs-profiles.el ends here
