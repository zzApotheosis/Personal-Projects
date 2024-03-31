(use-package eglot
  :ensure t)

(setq-default eglot-ignored-server-capabilities
	      '(:documentFormattingProvider
		:documentRangeFormattingProvider
		:documentOnTypeFormattingProvider))
