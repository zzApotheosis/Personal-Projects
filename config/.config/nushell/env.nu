# env.nu
#
# Installed by:
# version = "0.111.0"
#
# Previously, environment variables were typically configured in `env.nu`.
# In general, most configuration can and should be performed in `config.nu`
# or one of the autoload directories.
#
# This file is generated for backwards compatibility for now.
# It is loaded before config.nu and login.nu
#
# See https://www.nushell.sh/book/configuration.html
#
# Also see `help config env` for more options.
#
# You can remove these comments if you want or leave
# them for future reference.

use std/util 'path add'

def --env find-ssh-auth-sock [] {
    # Find SSH_AUTH_SOCK in ~/.ssh
    if not ('SSH_AUTH_SOCK' in $env) {
        try {
            let socket = (^find $"($env.HOME)/.ssh" -type s -print -quit)
            if ($socket | is-not-empty) {
                $env.SSH_AUTH_SOCK = ($socket | into string)
            }
        }
    }
    
    # Fallback to find SSH_AUTH_SOCK in /tmp
    if not ('SSH_AUTH_SOCK' in $env) {
        try {
            let socket = ls /tmp/ssh*/* | where type == 'socket' | first
            if $socket != null {
                $env.SSH_AUTH_SOCK = ($socket.name | into string)
            }
        }
    }
}

#$env.EDITOR = "nvim"
$env.EDITOR = 'hx'

let paths = [
    ($nu.home-dir | path join ".local/bin"), # User-installed local binaries
    ($nu.home-dir | path join ".cargo/bin"), # User-installed Cargo/Rust binaries
    ($nu.home-dir | path join ".nimble/bin") # User-installed Nim binaries
]
for path in $paths {
    path add $path
}


if not ('LANG' in $env) {
   $env.LANG = 'en_US.UTF-8'
}

# C Compiler
$env.CC = 'clang'
$env.CXX = 'clang++'
#$env.LD = 'lld'
$env.LD = 'ld.lld'
$env.AR = 'llvm-ar'
$env.NM = 'llvm-nm'
$env.STRIP = 'llvm-strip'
$env.OBJCOPY = 'llvm-objcopy'
$env.OBJDUMP = 'llvm-objdump'
$env.READELF = 'llvm-readelf'
$env.HOSTCC = 'clang'
$env.HOSTCXX = 'clang++'
$env.HOSTAR = 'llvm-ar'
$env.HOSTLD = 'lld'
$env.HOSTLD = 'ld.lld'

# Set the GPG_TTY so gpg commands can be used in the terminal
$env.GPG_TTY = (^tty | into string)

# I really want there to be an SSH agent running
find-ssh-auth-sock

# Make sure the Nushell cache directory exists
mkdir $"($nu.cache-dir)"

# Setup basic aliases
const alias_data = "# Shorthand for 'clear'
alias c = clear

# Shorthand for 'ls -a'
alias l = ls -a

# Shorthand for 'ls -al'
alias ll = ls -al

# Shorthand for 'emacsclient -c'
alias ec = emacsclient -c

# Shorthand for 'emacsclient -c -nw'
alias ecnw = emacsclient -c -nw

"

const aliases_file = ($nu.cache-dir | path join 'custom-aliases.nu')
$alias_data | save --force $aliases_file

# carapace-bin stuff
const carapace_cache_file = ($nu.cache-dir | path join 'carapace.nu')
if (which carapace | is-not-empty) {
    $env.CARAPACE_BRIDGES = 'zsh,fish,bash' # optional
    carapace _carapace nushell | save --force $carapace_cache_file
} else {
    # Touch an empty file if not available
    '' | save --force $carapace_cache_file
}

# Zoxide stuff
const zoxide_file = ($nu.cache-dir | path join 'zoxide.nu')
if (which zoxide | is-not-empty) {
    zoxide init nushell | save --force $zoxide_file

    # Update the custom aliases
    "# Shorthand for 'z'\nalias cd = z\n\n# Shorthand for 'zi'\nalias cdi = zi\n\n" | save --append $aliases_file
} else {
    # Touch an empty file if not available
    '' | save --force $zoxide_file
}

# fzf stuff
const fzf_file = ($nu.default-config-dir | path join 'autoload' 'fzf_integration.nu')
mkdir ($nu.default-config-dir | path join 'autoload')
if (which fzf | is-not-empty) {
    fzf --nushell | save --force $fzf_file
} else {
    # Touch an empty file if not available
    '' | save --force $fzf_file
}

