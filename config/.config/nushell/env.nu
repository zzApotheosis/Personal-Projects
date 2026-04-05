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

#$env.EDITOR = "nvim"
$env.EDITOR = 'hx'

$env.PATH = ($env.PATH | prepend "~/.local/bin")
$env.PATH = ($env.PATH | prepend "~/.cargo/bin")

if not ('LANG' in $env) {
   $env.LANG = 'en_US.UTF-8'
}

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

try {
    $env.CARAPACE_BRIDGES = 'zsh,fish,bash' # optional
    mkdir $"($nu.cache-dir)"
    carapace _carapace nushell | save --force $"($nu.cache-dir)/carapace.nu"
}

#if ( '~/.inshellisense/init/nu/init.nu' | path exists ) { source ~/.inshellisense/init/nu/init.nu }
