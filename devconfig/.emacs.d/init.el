;; Enable mouse support you fucking donkey
(unless (display-graphic-p)
  (xterm-mouse-mode t)
  )

;; Disable the startup screen you fucking donkey
(setq-default inhibit-startup-screen t)

;; Never use tabs, the menu bar, the tool bar, or the scroll bar you fucking donkey
(indent-tabs-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Define eshell history size
(setq-default eshell-history-size 25000)

;; Define manual load-path items
(add-to-list 'load-path "~/.emacs.d/pkgs/auto-complete")

;; Load manually installed packages
(use-package auto-complete)

;; Default to Emacs keybindings but have Evil mode installed
(evil-mode t)
(setq-default evil-default-state 'emacs)

;; Enable Vertico mode
(vertico-mode t)

;; Define tab-stop-list
(let ((limit 120)
      (step 4)
      (start 4))
  (setq-default tab-stop-list (number-sequence start limit step)))

;; Set C style
(setq c-default-style "gnu")

;; Define hook for c-mode-common
(add-hook 'c-mode-common-hook
          #'(lambda ()
              ;; Put stuff here
	      (eglot-ensure)
	      (auto-complete-mode t)
              ))

;; Define hook for prog-mode
(add-hook 'prog-mode-hook
          #'(lambda ()
              (hs-minor-mode t)
	      (display-line-numbers-mode t)
	      (column-number-mode t)
              ))

;; Define hook for dired-mode
(add-hook 'dired-mode-hook
          #'(lambda ()
              (interactive)
              (dired-hide-details-mode)
              ))

;; Customize backup location
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/backup/"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups
;; (defun make-backup-file-name (FILE)
;;   (let ((dirname (concat "~/.emacs.d/backup/"
;;                          (format-time-string "%y/%m/%d/"))))
;;     (if (not (file-exists-p dirname))
;;         (make-directory dirname t))
;;     (concat dirname (file-name-nondirectory FILE))))

;; Show matching braces
(show-paren-mode t)

;; Set smooth scrolling
(setq-default scroll-step 1)

;; ERC column width
(add-hook 'window-configuration-change-hook
          #'(lambda ()
              (setq erc-fill-column (- (window-width) 2))))
;; (make-variable-buffer-local 'erc-fill-column)
;; (add-hook 'window-configuration-change-hook 
;; 	  #'(lambda ()
;; 	      (save-excursion
;; 	        (walk-windows
;; 		 #'(lambda (w)
;; 		     (let ((buffer (window-buffer w)))
;; 		       (set-buffer buffer)
;; 		       (when (eq major-mode 'erc-mode)
;; 		         (setq erc-fill-column (- (window-width w) 2)))))))))

;; Custom settings from the (customize) function
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(package-selected-packages
   '(magit org rust-mode flycheck goto-chg eglot dash multishell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
