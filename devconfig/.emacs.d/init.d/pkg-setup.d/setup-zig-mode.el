(with-cd user-pkgs-dir
	 ;; Check for nim-mode
	 (let (
	       (repo-dir "zig-mode")
	       (cmd "git clone https://github.com/ziglang/zig-mode")
	       )
	   (unless (file-directory-p repo-dir)
	     (shell-command cmd)
	     )
	   )

	 ;; Check for zig-mode dependencies
	 (let (
	       (repo-dir "emacs-reformatter")
	       (cmd "git clone https://github.com/purcell/emacs-reformatter")
	       )
	   (unless (file-directory-p repo-dir)
	     (shell-command cmd)
	     )
	   )
	 )

(add-to-list 'load-path (concat user-pkgs-dir "/zig-mode/"))
(add-to-list 'load-path (concat user-pkgs-dir "/emacs-reformatter/"))

(load "zig-mode")

(setq-default pkg-installed-zig-mode-p t)
