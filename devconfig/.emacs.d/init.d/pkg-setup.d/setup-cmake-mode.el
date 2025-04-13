(with-cd user-pkgs-dir
	 ;; Check for cmake-mode
	 (let (
	       (repo-dir "CMake")
	       (cmd "git clone https://github.com/Kitware/CMake")
	       )
	   (unless (file-directory-p repo-dir)
	     (shell-command cmd)
	     )
	   )
	 )

(add-to-list 'load-path (concat user-pkgs-dir "/CMake/Auxiliary/"))

(load "cmake-mode")

(setq-default pkg-installed-cmake-mode-p t)
