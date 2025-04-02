(add-to-list 'load-path (concat user-pkgs-dir "/nim-mode/"))

(with-cd user-pkgs-dir
	 (let (
	       (repo-dir "commenter")
	       (cmd "git clone https://github.com/yuutayamada/commenter")
	       )
	   (message "got to 1")
	   (unless (file-directory-p repo-dir)
	     (message "got to 2")
	     (shell-command cmd)
	     )
	   )
	 )
(add-to-list 'load-path (concat user-pkgs-dir "/commenter/"))

(load "nim-mode")

(setq-default pkg-installed-nim-mode-p t)
