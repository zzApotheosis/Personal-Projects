;; -*- lexical-binding: t; -*-
(load-file (concat user-emacs-directory "/pkgs/nushell-mode/nushell-mode.el"))
(add-to-list 'auto-mode-alist '("\\.nu\\'" . nushell-mode))

(setq-default pkg-installed-nushell-mode-p t)
