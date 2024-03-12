;; Enable mouse support you fucking donkey
(unless (display-graphic-p)
  (xterm-mouse-mode)
  )

;; Disable the startup screen you fucking donkey
(setq-default inhibit-startup-screen t)

;; Never use tabs you fucking donkey
(setq-default indent-tabs-mode nil)

;; Don't use the menu bar you fucking donkey
(setq-default menu-bar-mode nil)

;; Don't use the scroll bar you fucking donkey
(setq-default scroll-bar-mode nil)

;; Define tab-stop-list
(let ((limit 120)
      (step 4)
      (start 4))
  (setq-default tab-stop-list (number-sequence start limit step)))

;; Set C style
(setq c-default-style "gnu")

;;(setq-default tab-stop-list (number-sequence ))

(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode and related modes
  (setq column-number-mode t)
  ;;(c-mode-tab-stop-list)
  ;;(setq indent-line-function 'insert-tab)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Customize backup location
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/backup/"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; Show matching braces
(setq-default show-paren-mode t)

;; Show column numbers
(setq-default column-number-mode t)

;; Set smooth scrolling
(setq-default scroll-step 1)

;; ERC column width
;; (add-hook 'window-configuration-change-hook
;;           #'(lambda ()
;;              (setq erc-fill-column (- (window-width) 2))))
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook 
	  #'(lambda ()
	      (save-excursion
	        (walk-windows
		 (lambda (w)
		   (let ((buffer (window-buffer w)))
		     (set-buffer buffer)
		     (when (eq major-mode 'erc-mode)
		       (setq erc-fill-column (- (window-width w) 2)))))))))
