;; Add C style, but default to gnu
(c-add-style "z"
	     '("gnu"
	       (c-basic-offset . 4)
               (c-offsets-alist
		(substatement-open . 0)
		)
	       ))
(setq c-default-style "gnu")
