;; Define variables to use later
(setq-default pkg-installed-company-p nil)
(setq-default pkg-installed-compat-p nil)
(setq-default pkg-installed-dash-p nil)
(setq-default pkg-installed-eglot-p nil)
(setq-default pkg-installed-eldoc-p nil)
(setq-default pkg-installed-erc-p nil)
(setq-default pkg-installed-evil-p nil)
(setq-default pkg-installed-flycheck-p nil)
(setq-default pkg-installed-goto-chg-p nil)
(setq-default pkg-installed-magit-p nil)
(setq-default pkg-installed-multishell-p nil)
(setq-default pkg-installed-neotree-p nil)
(setq-default pkg-installed-org-p nil)
(setq-default pkg-installed-protobuf-p nil)
(setq-default pkg-installed-rust-mode-p nil)
(setq-default pkg-installed-spacemacs-common-p nil)
(setq-default pkg-installed-vertico-p nil)
(setq-default pkg-installed-vterm-p nil)
(setq-default pkg-installed-yasnippet-p nil)
(setq-default pkg-installed-go-mode-p nil)
(setq-default pkg-installed-d-mode-p nil)
(setq-default pkg-installed-nim-mode-p nil)
(setq-default pkg-installed-zig-mode-p nil)
(setq-default pkg-installed-fakepkg-p nil)
(setq-default pkg-installed-markdown-mode-p nil)

(let (
      (pkg-setup-dir (concat user-emacs-directory "/init.d/pkg-setup.d/"))
      (user-pkgs-dir (concat user-emacs-directory "/pkgs/"))
      (setup-files '("setup-spacemacs-theme.el"
		     "setup-company.el"
		     "setup-dash.el"
		     "setup-evil.el"
		     "setup-flycheck.el"
		     "setup-goto-chg.el"
		     "setup-magit.el"
		     "setup-multishell.el"
		     "setup-org.el"
		     "setup-rust-mode.el"
		     "setup-vertico.el"
		     "setup-yasnippet.el"
		     "setup-erc.el"
		     "setup-eshell.el"
		     "setup-go-mode.el"
		     "setup-d-mode.el"
		     "setup-zig-mode.el"
		     "setup-markdown-mode.el"))
      )

  ;; Load user-defined setup files
  (dolist (setup-file setup-files)
    (load-file (concat pkg-setup-dir "/" setup-file))
    )

  ;; Non GNU ELPA packages (generally these are on MELPA but I'd prefer to manage them myself. See emacs-setup.pl)
  (when (or
	 (string-equal system-type "gnu")
	 (string-equal system-type "gnu/linux")
	 (string-equal system-type "darwin")
	 )
    ;; lmao Windows you suuuuuucccccckkkkkk
    (load-file (concat pkg-setup-dir "/setup-vterm.el"))
    )
  (load-file (concat pkg-setup-dir "/setup-neotree.el"))
  (load-file (concat pkg-setup-dir "/setup-protobuf.el"))
  (load-file (concat pkg-setup-dir "/setup-eldoc-box.el"))
  (load-file (concat pkg-setup-dir "/setup-nim-mode.el"))
  )
