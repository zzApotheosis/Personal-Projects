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

$env.EDITOR = "nvim"

$env.PATH ++= ["~/.local/bin"]
$env.PATH ++= ["~/.cargo/bin"]

#$env.LANG = 'en_US.UTF-8'

# C Compiler
$env.CC = "clang"
$env.CXX = "clang++"
#$env.LD = "lld"
$env.LD = "ld.lld"
$env.AR = "llvm-ar"
$env.NM = "llvm-nm"
$env.STRIP = "llvm-strip"
$env.OBJCOPY = "llvm-objcopy"
$env.OBJDUMP = "llvm-objdump"
$env.READELF = "llvm-readelf"
$env.HOSTCC = "clang"
$env.HOSTCXX = "clang++"
$env.HOSTAR = "llvm-ar"
$env.HOSTLD = "lld"
$env.HOSTLD = "ld.lld"

$env.GPG_TTY = (^tty | into string)

try {
    let agents = ps | where name == 'ssh-agent' | first
    let sockets = ls /tmp/ssh*/* | where type == 'socket' | first
    if $agents != null {
        $env.SSH_AGENT_PID = ($agents.pid | into string)
        $env.SSH_AUTH_SOCK = ($sockets.name | into string)
    }
}

# Starship!
try {
    mkdir ($nu.data-dir | path join "vendor/autoload")
    starship init nu | save -f ($nu.data-dir | path join "vendor/autoload/starship.nu")
}

# oh-my-posh
#try {
#    oh-my-posh init nu # --config ($env.HOME | path join "/.config/omp-theme.json")
#}

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
alias ec = emacsclient -c
alias ecnw = emacsclient -c -nw
