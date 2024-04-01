;; Initialize GNU Emacs
(let (
      (init-dir (concat user-emacs-directory "/init.d/"))
      )
  (load-file (concat init-dir "/basic-settings.el"))
  (load-file (concat init-dir "/pkg-setup.el"))
  (load-file (concat init-dir "/c-style-preferences.el"))
  (load-file (concat init-dir "/enable-commands.el"))
  (load-file (concat init-dir "/hooks.el"))
  (load-file (concat init-dir "/keybindings.el"))
  )

;; Custom settings from the (customize) function
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck evil vertico yasnippet company magit org rust-mode goto-chg eglot dash multishell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
