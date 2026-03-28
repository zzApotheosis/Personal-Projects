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


# Starship!
#try {
#    mkdir ($nu.data-dir | path join "vendor/autoload")
#    starship init nu | save -f ($nu.data-dir | path join "vendor/autoload/starship.nu")
#}

# oh-my-posh
try {
    oh-my-posh init nu --config ($env.HOME | path join '.config/omp-theme.json')
}

# carapace-bin
try {
    ## ${UserConfigDir}/nushell/env.nu
    #$env.CARAPACE_BRIDGES = 'zsh,fish,bash,inshellisense' # optional
    mkdir "$nu.cache-dir"
    carapace _carapace nushell | save --force "$nu.cache-dir/carapace.nu"
    
    # ${UserConfigDir}/nushell/config.nu
    source "$nu.cache-dir/carapace.nu"
}

# Interface to the system-wide lsd command
def --wrapped lsd [...args] {
     if ($args | length) > 0 {
        ^lsd ...$args
        return
     }

     ^lsd -lah
}

# Set custom aliases
alias c = clear
alias l = ls -a
alias ll = ls -al
alias ec = emacsclient -c
alias ecnw = emacsclient -c -nw
