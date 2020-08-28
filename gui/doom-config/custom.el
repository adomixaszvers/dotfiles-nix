;;; ~/.config/doom/custom.el -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("53f8223005ceb058848fb92c2c4752ffdfcd771f8ad4324b3d0a4674dec56c44" "8c75e2bdf8d1293c77a752dd210612cfb99334f7edd360a42a58a8497a078b35" "e47c0abe03e0484ddadf2ae57d32b0f29f0b2ddfe7ec810bd6d558765d9a6a6c" "5091eadbb87fa0a168a65f2c3e579d1a648d764f12ab9d3ab7bdefca709cd2a5" "4a9f595fbffd36fe51d5dd3475860ae8c17447272cf35eb31a00f9595c706050" "1897b97f63e91a792e8540c06402f29d5edcbfb0aafd64b1b14270663d6868ee" "f2b83b9388b1a57f6286153130ee704243870d40ae9ec931d0a1798a5a916e76" default)))
 '(display-line-numbers-type (quote visual))
 '(org-agenda-files
   (quote
    ("/home/adomas/fast/emacs-doom/docs/getting_started.org" "/home/adomas/fast/emacs-doom/docs/contributing.org" "/home/adomas/fast/emacs-doom/docs/faq.org")))
 '(safe-local-variable-values
   (quote
    ((dante-project-root . "/home/adomas/.config/nixpkgs")
     (dante-methods new-impure-nix)
     (intero-targets "accumulate:lib" "accumulate:test:test")
     (intero-targets "twelve-days:lib" "twelve-days:test:test")
     (flycheck-disabled-checkers quote
                                 (emacs-lisp-checkdoc))
     (projectile-project-compilation-cmd . "home-manager switch")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
