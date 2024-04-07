(use-package erc
  :ensure t)

;; ERC column width
(add-hook 'window-configuration-change-hook
          (lambda ()
            (setq erc-fill-column (- (window-width) 2))))
;;(make-variable-buffer-local 'erc-fill-column)
;;(add-hook 'window-configuration-change-hook 
;;	  (lambda ()
;;	    (save-excursion
;;	      (walk-windows
;;	       (lambda (w)
;;		 (let ((buffer (window-buffer w)))
;;		   (set-buffer buffer)
;;		   (when (eq major-mode 'erc-mode)
;;		     (setq erc-fill-column (- (window-width w) 2)))))))))

;; We want to kill buffers on /part
(setq-default erc-kill-buffer-on-part t)

;; Load custom IRC settings in private file
;; NOTE: This probably is a bad approach to this problem. I need to learn how auth-sources works and do that instead. So I'll leave this commented out for now
;;(let (
;;      (erc-creds-file (concat init-dir "/erc-private.el"))
;;      )
;;  (when (file-exists-p erc-creds-file)
;;    (load-file erc-creds-file)
;;    )
;;  )
