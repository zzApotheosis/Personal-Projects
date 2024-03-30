;; Initialize GNU Emacs
(make-variable-buffer-local 'load-path)

(add-to-list 'load-path (concat user-emacs-directory "/init.d/"))

(load "basic-settings")
(load "pkg-setup")
(load "c-style-preferences")
(load "enable-commands")
(load "hooks")
(load "keybindings")

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
