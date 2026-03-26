(load-file (concat user-pkgs-dir "/emacs-fish/fish-mode.el"))
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))

(setq-default pkg-installed-fish-mode-p t)
