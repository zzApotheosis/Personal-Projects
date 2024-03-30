;; Add extra load-path items
;;(add-to-list 'load-path "~/.emacs.d/pkgs/auto-complete")
(let (
      (user-pkgs-dir (concat user-emacs-directory "/pkgs/"))
      )
  (make-variable-buffer-local 'load-path)
  
  (add-to-list 'load-path (concat user-pkgs-dir "/emacs-neotree/"))
  (add-to-list 'load-path (concat user-emacs-directory "/init.d/pkg-setup.d/"))

  (load "setup-erc")
  (load "setup-neotree")
  (load "setup-evil")
  (load "setup-eglot")
  )
