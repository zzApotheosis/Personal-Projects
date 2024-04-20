(use-package eglot
  :ensure t)

(setq-default eglot-ignored-server-capabilities
	      '(:documentFormattingProvider
		:documentRangeFormattingProvider
		:documentOnTypeFormattingProvider))

(setq-default pkg-installed-eglot-p t)
