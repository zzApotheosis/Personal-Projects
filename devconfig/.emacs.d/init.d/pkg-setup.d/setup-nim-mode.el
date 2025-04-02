(with-cd user-pkgs-dir
	 ;; Check for nim-mode
	 (let (
	       (repo-dir "nim-mode")
	       (cmd "git clone https://github.com/nim-lang/nim-mode")
	       )
	   (unless (file-directory-p repo-dir)
	     (shell-command cmd)
	     )
	   )

	 ;; Check for nim-mode dependencies
	 (let (
	       (repo-dir "commenter")
	       (cmd "git clone https://github.com/yuutayamada/commenter")
	       )
	   (unless (file-directory-p repo-dir)
	     (shell-command cmd)
	     )
	   )
	 )

(add-to-list 'load-path (concat user-pkgs-dir "/nim-mode/"))
(add-to-list 'load-path (concat user-pkgs-dir "/commenter/"))

(load "nim-mode")

(setq-default pkg-installed-nim-mode-p t)
