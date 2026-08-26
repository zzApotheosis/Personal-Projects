;; Define variables to use later
(setq-default pkg-installed-company-p nil
              pkg-installed-compat-p nil
              pkg-installed-dash-p nil
              pkg-installed-eglot-p nil
              pkg-installed-eldoc-p nil
              pkg-installed-erc-p nil
              pkg-installed-evil-p nil
              pkg-installed-goto-chg-p nil
              pkg-installed-magit-p nil
              pkg-installed-multishell-p nil
              pkg-installed-neotree-p nil
              pkg-installed-org-p nil
              pkg-installed-protobuf-p nil
              pkg-installed-rust-mode-p nil
              pkg-installed-spacemacs-common-p nil
              pkg-installed-vertico-p nil
              pkg-installed-vterm-p nil
              pkg-installed-yasnippet-p nil
              pkg-installed-go-mode-p nil
              pkg-installed-d-mode-p nil
              pkg-installed-nim-mode-p nil
              pkg-installed-zig-mode-p nil
              pkg-installed-markdown-mode-p nil
	      pkg-installed-cmake-mode-p nil
              pkg-installed-fish-mode-p nil
              pkg-installed-nushell-mode-p nil
              pkg-installed-haskell-mode-p nil
              
              pkg-installed-fakepkg-p nil)

(let (
      (pkg-setup-dir (concat user-emacs-directory "/init.d/pkg-setup.d/"))
      (user-pkgs-dir (concat user-emacs-directory "/pkgs/"))
      (setup-files '("setup-spacemacs-theme.el"
		     "setup-company.el"
		     "setup-dash.el"
                     "setup-eglot.el"
		     "setup-evil.el"
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
		     "setup-markdown-mode.el"
                     "setup-haskell-mode.el"
		     ))
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
  (load-file (concat pkg-setup-dir "/setup-cmake-mode.el"))
  (load-file (concat pkg-setup-dir "/setup-neotree.el"))
  (load-file (concat pkg-setup-dir "/setup-protobuf.el"))
  (load-file (concat pkg-setup-dir "/setup-eldoc-box.el"))
  (load-file (concat pkg-setup-dir "/setup-nim-mode.el"))
  (load-file (concat pkg-setup-dir "/setup-fish-mode.el"))
  (load-file (concat pkg-setup-dir "/setup-nushell-mode.el"))
  )
