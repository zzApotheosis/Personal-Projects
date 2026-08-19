# Customize PATH
PATH=$(printf "%s" "${PATH}" | sed "s@:${HOME}/usr/local/bin@@" | sed "s@${HOME}/usr/local/bin:@@")
[ -d "${HOME-}/.cargo/bin/" ] && PATH="${HOME}/.cargo/bin:${PATH}"
[ -d "${HOME-}/.local/bin/" ] && PATH="${HOME}/.local/bin:${PATH}"
export PATH

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
if [ -z "${GPG_TTY-}" ] && [ -n "${TTY-}" ]; then
  export GPG_TTY="${TTY}"
fi
if [ -z "${GPG_TTY-}" ]; then
  export GPG_TTY="$(tty)"
fi

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
