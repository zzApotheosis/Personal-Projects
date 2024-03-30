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

(vertico-mode t)
(global-ede-mode t)

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

