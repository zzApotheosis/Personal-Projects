;; Add extra load-path items
;;(add-to-list 'load-path "~/.emacs.d/pkgs/auto-complete")
(setq user-pkgs-dir (concat user-emacs-directory "/pkgs/"))

(let (
      (pkg-setup-dir (concat user-emacs-directory "/init.d/pkg-setup.d/"))
      )
  (load-file (concat pkg-setup-dir "/setup-company.el"))
  (load-file (concat pkg-setup-dir "/setup-dash.el"))
  (load-file (concat pkg-setup-dir "/setup-eglot.el"))
  (load-file (concat pkg-setup-dir "/setup-evil.el"))
  (load-file (concat pkg-setup-dir "/setup-flycheck.el"))
  (load-file (concat pkg-setup-dir "/setup-goto-chg.el"))
  (load-file (concat pkg-setup-dir "/setup-magit.el"))
  (load-file (concat pkg-setup-dir "/setup-multishell.el"))
  (load-file (concat pkg-setup-dir "/setup-org.el"))
  (load-file (concat pkg-setup-dir "/setup-rust-mode.el"))
  (load-file (concat pkg-setup-dir "/setup-vertico.el"))
  (load-file (concat pkg-setup-dir "/setup-yasnippet.el"))
  (load-file (concat pkg-setup-dir "/setup-erc.el"))
  
  (load-file (concat pkg-setup-dir "/setup-neotree.el"))
  (load-file (concat pkg-setup-dir "/setup-vterm.el"))
  )
