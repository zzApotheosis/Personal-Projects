;; Define hook for c-mode-common
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Put stuff here
	    (when pkg-installed-eglot-p
	      (eglot-ensure))
	    (when pkg-installed-yasnippet-p
	      (yas-minor-mode t))
	    (when pkg-installed-flycheck-p
              (flycheck-mode t))
	    (when pkg-installed-eldoc-box-p
	      (eldoc-box-hover-at-point-mode t))
            ))

;; Define hook for prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode t)
	    (display-line-numbers-mode t)
	    (column-number-mode t)
	    (when pkg-installed-company-p
	      (company-mode t)
	      )
            ))

;; Define hook for dired-mode
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode t)
            ))

;; Define hook for Man-mode
(add-hook 'Man-mode-hook
	  (lambda ()
	    (setq Man-notify-method 'pushy)
	    ))

(when pkg-installed-fakepkg-p
  (message "LIGMA"))
