# Customize PATH
PATH=$(printf "%s" "${PATH}" | sed "s@:${HOME}/usr/local/bin@@" | sed "s@${HOME}/usr/local/bin:@@")
PATH="${HOME}/.local/bin:${PATH}"
export PATH

# C Compiler
# Default to GCC but have Clang available. The more, the merrier!
#export CC="clang"
#export CXX="clang++"
#export LD="lld"
##export LD="ld.lld"
#export AR="llvm-ar"
#export NM="llvm-nm"
#export STRIP="llvm-strip"
#export OBJCOPY="llvm-objcopy"
#export OBJDUMP="llvm-objdump"
#export READELF="llvm-readelf"
#export HOSTCC="clang"
#export HOSTCXX="clang++"
#export HOSTAR="llvm-ar"
#export HOSTLD="lld"
##export HOSTLD="ld.lld"
#export CC="zig cc"
#export CXX="zig c++"

# GnuPG TTY
export GPG_TTY=${TTY}

# Declare Preferred Editor
export EDITOR="nvim"
#export EDITOR="hx"
#export EDITOR="emacs"
#export EDITOR="emacsclient -s ${HOME}/.emacs.d/.socket -c"
