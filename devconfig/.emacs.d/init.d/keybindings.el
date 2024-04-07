;; Create personal keybindings

;; A convenience keybinding for switching between emacs mode and evil mode when evil-mode is enabled
(keymap-global-set "ESC ESC z" 'evil-local-mode)

;; A keybinding to quit Emacs, but also kill the daemon with the client
(keymap-global-set "C-x c k" 'save-buffers-kill-emacs)
