# Customize PATH
PATH=$(printf "%s" "${PATH}" | sed "s@:${HOME}/usr/local/bin@@" | sed "s@${HOME}/usr/local/bin:@@")
[ -d "${HOME}/.cargo/bin/" ] && PATH="${HOME}/.cargo/bin:${PATH}"
[ -d "${HOME}/.local/bin/" ] && PATH="${HOME}/.local/bin:${PATH}"
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
export GPG_TTY=${TTY}

# Declare Preferred Editor
#export EDITOR="emacs"
#export EDITOR="nvim"
export EDITOR="hx"
#export EDITOR="emacsclient -s ${HOME}/.emacs.d/.socket -c"

# If we have an ssh-agent running, let's automatically set up our environment to use it
#_ssh_agent_pid=`pidof ssh-agent`
if [ "${SSH_AUTH_SOCK}" = "" ]; then
  #export SSH_AGENT_PID="${_ssh_agent_pid}"
  export SSH_AUTH_SOCK=`find "${HOME}/.ssh" -type s -name '*agent*' -print -quit`
fi
if [ "${SSH_AUTH_SOCK}" = "" ]; then
  _ssh_dir=`find /tmp -mindepth 1 -maxdepth 1 -type d -name 'ssh*' -print -quit`
  if [ -d "${_ssh_dir}" ]; then
    export SSH_AUTH_SOCK=`find "${_ssh_dir}" -type s -name 'agent.*' -print -quit`
  fi
fi
