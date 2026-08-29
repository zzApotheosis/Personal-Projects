;; -*- lexical-binding: t; -*-
(load-file (concat user-emacs-directory "/pkgs/protobuf/editors/protobuf-mode.el"))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(setq-default pkg-installed-protobuf-p t)
