;; -*- lexical-binding: t; -*-
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (setq pcomplete-cycle-completions nil)
	    ))
