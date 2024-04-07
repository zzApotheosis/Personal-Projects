;; Create personal keybindings

;; All Evil related keybindings
;; A convenience keybinding for switching between emacs mode and evil mode when evil-mode is enabled
(keymap-global-set "C-x c e l" 'evil-local-mode)
;; A convenience keybinding for evil-mode
(keymap-global-set "C-x c e e" 'evil-mode)

;; A keybinding to quit Emacs, but also kill the daemon with the client
(keymap-global-set "C-x c k" 'save-buffers-kill-emacs)

;; All "theme" related keybindings
;; A keybinding to disable all themes (useful on the terminal when I want to let my terminal emulator handle colors)
(keymap-global-set "C-x c t d" 'disable-all-themes)
(keymap-global-set "C-x c t c" 'customize-themes)
