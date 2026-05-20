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

# Update the PATH environment variable by prepending or appending a new directory
def --env update-path [
    new_path: string
    --append (-a) # Append instead of prepend (default is prepend)
] {
    # Convert the string to an absolute path immediately
    let absolute_path = ($new_path | path expand)

    # 1. Early exit if the path doesn't actually exist on disk
    if not ($absolute_path | path exists) {
        print -e $"Warning: Path ($absolute_path) does not exist."
        return
    }

    # 2. Early exit if it's already a member of the PATH list
    if $absolute_path in $env.PATH {
        return
    }

    # 3. Flat scope execution: perform the list mutation
    if $append {
        $env.PATH = ($env.PATH | append $absolute_path)
    } else {
        $env.PATH = ($env.PATH | prepend $absolute_path)
    }
}

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
    ($env.HOME | path join ".local/bin"),
    ($env.HOME | path join ".cargo/bin")
]
for path in $paths {
    update-path $path
}


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

# I really want there to be an SSH agent running
find-ssh-auth-sock

let carapace_cache_file = ($nu.cache-dir | path join "carapace.nu")
if (which carapace | is-not-empty) {
    $env.CARAPACE_BRIDGES = 'zsh,fish,bash' # optional
    mkdir $"($nu.cache-dir)"
    carapace _carapace nushell | save --force $carapace_cache_file
} else {
    # Touch an empty file if not available
    "" | save --force $carapace_cache_file
}
