(add-hook 'eshell-mode-hook
	  (lambda ()
	    (setq pcomplete-cycle-completions nil)
	    ))
