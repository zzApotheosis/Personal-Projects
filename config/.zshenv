# Utility function to update PATH
path_add()
{
    # Determine function mode
    case $1 in
        prepend|append) ;;
        *)
            printf 'usage: path_add prepend|append directory\n' >&2
            return 2
            ;;
    esac

    # Ensure only two arguments were passed to this function
    [ "$#" -eq 2 ] || {
        printf 'usage: path_add prepend|append directory\n' >&2
        return 2
    }

    # Simple input validation; only one path is allowed at a time
    case $2 in
        *:*)
            printf "path_add: directory must not contain ':'\n" >&2
            return 2
            ;;
    esac

    # Invoke AWK to process the PATH updates
    PATH=$(
        awk -v path="$PATH" -v mode="$1" -v entry="$2" '
            function add(x) {
                if (!seen[x]++) {
                    if (out++) printf ":"
                    printf "%s", x
                }
            }

            BEGIN {
                if (mode == "prepend")
                    add(entry)

                n = split(path, parts, ":")
                for (i = 1; i <= n; i++)
                    add(parts[i])

                if (mode == "append")
                    add(entry)
            }
        '
        )

    # Export the new PATH variable
    export PATH
}


# Customize PATH
path_add prepend "${HOME-}/node_modules/.bin"
path_add prepend "${HOME-}/.cargo/bin"
path_add prepend "${HOME-}/.local/bin"
unset -f path_add

# C Compiler
export CC="clang"
export CXX="clang++"
#export LD="lld"
export LD="ld.lld"
export AR="llvm-ar"
export NM="llvm-nm"
export STRIP="llvm-strip"
export OBJCOPY="llvm-objcopy"
export OBJDUMP="llvm-objdump"
export READELF="llvm-readelf"
export HOSTCC="clang"
export HOSTCXX="clang++"
export HOSTAR="llvm-ar"
export HOSTLD="lld"
export HOSTLD="ld.lld"

# GnuPG TTY
[ -z "${GPG_TTY-}" ] && [ -n "${TTY-}" ] && export GPG_TTY="${TTY}"
[ -z "${GPG_TTY-}" ] && export GPG_TTY="$(tty)"

# Declare Preferred Editor
#export EDITOR="emacs"
#export EDITOR="nvim"
export EDITOR="hx"
#export EDITOR="emacsclient -s ${HOME}/.emacs.d/.socket -c"

# If we have an ssh-agent running, let's automatically set up our environment to use it,
# but don't overwrite existing values
if [ -z "${SSH_AUTH_SOCK-}" ]; then
    SSH_AUTH_SOCK=$(find "${HOME-}/.ssh" -type s -name '*agent*' -print -quit 2> /dev/null)
    if [ -S "${SSH_AUTH_SOCK-}" ]; then
        export SSH_AUTH_SOCK
    else
        unset SSH_AUTH_SOCK
    fi
fi
if [ -z "${SSH_AUTH_SOCK-}" ]; then
    SSH_AUTH_SOCK=$(find /tmp -type s -name '*agent*' -print -quit 2> /dev/null)
    if [ -S "${SSH_AUTH_SOCK-}" ]; then
        export SSH_AUTH_SOCK
    else
        unset SSH_AUTH_SOCK
    fi
fi

# Declare umask
umask 0077

# Set up FNM if available
command -v fnm > /dev/null 2>&1 && eval $(fnm env)
