;; Initialize GNU Emacs
(add-to-list 'load-path (concat user-emacs-directory "init.d"))
(load "basic-settings.el")
(load "pkg-setup.el")
(load "c-style-preferences.el")
(load "hooks.el")

;; Load manually installed packages
;;(use-package auto-complete)
(use-package neotree)

;; Default to Emacs keybindings and leave Evil mode disabled but have it installed and available
(setq evil-default-state 'emacs)
;;(evil-mode t)

;; Create personal keybindings
(keymap-global-set "ESC ESC z" 'evil-local-mode)

;; Custom settings from the (customize) function
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(eglot-ignored-server-capabilities
   '(:documentFormattingProvider :documentRangeFormattingProvider :documentOnTypeFormattingProvider))
 '(package-selected-packages
   '(flycheck evil vertico yasnippet company magit org rust-mode goto-chg eglot dash multishell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Enable certain commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
