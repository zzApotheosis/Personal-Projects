;; Initialize GNU Emacs
(let (
      (init-dir (concat user-emacs-directory "/init.d/"))
      )
  (load-file (concat init-dir "/functions.el"))
  (load-file (concat init-dir "/basic-settings.el"))
  (load-file (concat init-dir "/pkg-setup.el"))
  (load-file (concat init-dir "/c-style-preferences.el"))
  (load-file (concat init-dir "/enable-commands.el"))
  (load-file (concat init-dir "/hooks.el"))
  (load-file (concat init-dir "/keybindings.el"))
  (load-file (concat init-dir "/theme.el"))
  )

;; Custom settings from the (customize) function
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(spacemacs-theme flycheck evil vertico yasnippet company magit org rust-mode goto-chg eglot dash multishell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
