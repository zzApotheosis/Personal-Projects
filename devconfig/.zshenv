# Customize PATH
PATH=$(printf "%s" "${PATH}" | sed "s@:${HOME}/usr/local/bin@@" | sed "s@${HOME}/usr/local/bin:@@")
PATH="${HOME}/.local/bin:${PATH}"
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
export EDITOR="emacs"
#export EDITOR="hx"
#export EDITOR="nvim"
#export EDITOR="emacsclient -s ${HOME}/.emacs.d/.socket -c"
