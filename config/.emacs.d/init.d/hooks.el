;; Define common hook for text-based buffers  -*- lexical-binding: t; -*-
(add-hook 'text-mode-hook
          (lambda ()
            (when (display-graphic-p)
              (hl-line-mode nil)
              )
            )
          )

;; Define hook for c-mode-common
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Put stuff here
            (when pkg-installed-eglot-p
              (eglot-ensure))
            (when pkg-installed-yasnippet-p
              (yas-minor-mode t))
            (when pkg-installed-eldoc-box-p
              (eldoc-box-hover-at-point-mode t))
            ))

;; Define hook for rust-mode
(when pkg-installed-rust-mode-p
  (add-hook 'rust-mode-hook
            (lambda ()
              (when pkg-installed-eglot-p
                (eglot-ensure)
                )
              )
            )
  )

;; Define hook for python-mode
(add-hook 'python-mode-hook
          (lambda ()
            (when pkg-installed-eglot-p
              (eglot-ensure)
              )
            )
          )

;; Define hook for zig-mode
(when pkg-installed-zig-mode-p
  (add-hook 'zig-mode-hook
            (lambda ()
              (when pkg-installed-eglot-p
                (eglot-ensure)
                )
              )
            )
  )

;; Define hook for prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode t)
            (display-line-numbers-mode t)
            (column-number-mode t)
            (when pkg-installed-company-p
              (company-mode t)
              )
            (when (display-graphic-p)
              (hl-line-mode nil)
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
