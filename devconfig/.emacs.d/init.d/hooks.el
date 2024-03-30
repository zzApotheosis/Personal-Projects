;; Define hook for c-mode-common
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Put stuff here
	    (eglot-ensure)
	    (yas-minor-mode t)
            (flycheck-mode t)
            ))

;; Define hook for prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode t)
	    (display-line-numbers-mode t)
	    (column-number-mode t)
	    (company-mode t)
            ))

;; Define hook for dired-mode
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode t)
            ))

(add-hook 'Man-mode-hook
	  (lambda ()
	    (setq Man-notify-method 'pushy)
	    ))
