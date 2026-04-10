# Helper functions
function prepend_path --argument new_path
    if not contains -- $new_path $PATH
        set -gx PATH $new_path $PATH
    end
end

# Environment setup
if test -d "$HOME/.local/bin"
    prepend_path "$HOME/.local/bin"
end

if test -d "$HOME/.cargo/bin"
    prepend_path "$HOME/.cargo/bin"
end

# C Compiler
set -gx CC clang
set -gx CXX 'clang++'
#set -gx LD 'lld'
set -gx LD 'ld.lld'
set -gx AR llvm-ar
set -gx NM llvm-nm
set -gx STRIP llvm-strip
set -gx OBJCOPY llvm-objcopy
set -gx OBJDUMP llvm-objdump
set -gx READELF llvm-readelf
set -gx HOSTCC clang
set -gx HOSTCXX 'clang++'
set -gx HOSTAR llvm-ar
set -gx HOSTLD lld
set -gx HOSTLD 'ld.lld'

# GnuPG TTY
set -gx GPG_TTY $(tty)

# Declare Preferred Editor
#set -gx EDITOR "emacs"
#set -gx EDITOR "nvim"
set -gx EDITOR hx
#set -gx EDITOR "emacsclient -s ${HOME}/.emacs.d/.socket -c"

# If we have an ssh-agent running, let's automatically set up our environment to use it
set _ssh_agent_pid "$(pidof ssh-agent)"
if test -n "$_ssh_agent_pid"
    set -gx SSH_AGENT_PID "$_ssh_agent_pid"
    set -gx SSH_AUTH_SOCK $(find /tmp/ssh* -name 'agent.*' -print -quit)
end
set -e _ssh_agent_pid

# Interactive sessions only
if status is-interactive
    # Commands to run in interactive sessions can go here
    starship init fish | source
end
