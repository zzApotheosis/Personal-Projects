;; A function to toggle-off all recognized themes
(defun disable-all-themes ()
  "A function to disable all currently recognized themes."
  (interactive)
  (let (
	(themes (custom-available-themes))
	(result)
	)
    (dolist (element themes result)
      ;;(message element)
      (disable-theme element)
      )
    )
  )
