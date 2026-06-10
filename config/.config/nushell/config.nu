# config.nu
#
# Installed by:
# version = "0.111.0"
#
# This file is used to override default Nushell settings, define
# (or import) custom commands, or run any other startup tasks.
# See https://www.nushell.sh/book/configuration.html
#
# Nushell sets "sensible defaults" for most configuration settings, 
# so your `config.nu` only needs to override these defaults if desired.
#
# You can open this file in your default editor using:
#     config nu
#
# You can also pretty-print and page through the documentation for configuration
# options using:
#     config nu --doc | nu-highlight | less -R

# Set my umask
umask rwx------

# Starship!
if (which starship | is-not-empty) {
    mkdir ($nu.data-dir | path join "vendor/autoload")
    starship init nu | save -f ($nu.data-dir | path join "vendor/autoload/starship.nu")
}

# carapace-bin
source ($nu.cache-dir | path join 'carapace.nu')

# Zoxide
source ($nu.cache-dir | path join 'zoxide.nu')

# Interface to the system-wide lsd command
def --wrapped lsd [...args] {
     if ($args | length) > 0 {
        ^lsd ...$args
        return
     }

     ^lsd -lah
}

# Shorthand for 'clear'
alias c = clear

# Shorthand for 'ls -a'
alias l = ls -a

# Shorthand for 'ls -al'
alias ll = ls -al

# Shorthand for 'emacsclient -c'
alias ec = emacsclient -c

# Shorthand for 'emacsclient -c -nw'
alias ecnw = emacsclient -c -nw
