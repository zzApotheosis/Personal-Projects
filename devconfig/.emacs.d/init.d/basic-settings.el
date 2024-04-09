;; These are the basic settings
(unless (display-graphic-p) (xterm-mouse-mode t))
(setq-default inhibit-startup-screen t)
(indent-tabs-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq-default eshell-history-size 25000)
(global-auto-revert-mode t)
(show-paren-mode t)
(global-ede-mode t)
(tab-bar-mode t)

;; Confirm when quitting emacs
(setq-default confirm-kill-emacs #'y-or-n-p)

;; Set smooth scrolling
(setq-default scroll-step 1)

;; Define tab-stop-list
(let ((limit 120)
      (step 4)
      (start 4))
  (setq-default tab-stop-list (number-sequence start limit step)))

;; Customize backup location
(let (
      (backup-dir (concat user-emacs-directory "backup/" (format-time-string "%Y/%m/%d/")))
      )
  (when (not (file-directory-p backup-dir))
    (make-directory backup-dir t))
  (setq
   backup-by-copying t
   backup-directory-alist (list (cons "." backup-dir))
   delete-old-versions t
   kept-new-versions 4
   kept-old-versions 2
   version-control t)
  )
