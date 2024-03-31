;; Add extra load-path items
;;(add-to-list 'load-path "~/.emacs.d/pkgs/auto-complete")
(setq user-pkgs-dir (concat user-emacs-directory "/pkgs/"))

(add-to-list 'load-path (concat user-emacs-directory "/init.d/pkg-setup.d/"))

(load "setup-vertico")
(load "setup-erc")
(load "setup-neotree")
(load "setup-evil")
(load "setup-eglot")
(load "setup-vterm")
(load "setup-flycheck")
(load "setup-company")
(load "setup-magit")
