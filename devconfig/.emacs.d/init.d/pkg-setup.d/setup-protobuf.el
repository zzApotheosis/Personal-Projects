(load-file (concat user-pkgs-dir "/protobuf/editors/protobuf-mode.el"))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
