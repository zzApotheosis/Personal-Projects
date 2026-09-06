# Oh My Zsh setup
ZSH="$HOME/.oh-my-zsh"
if [ -e $ZSH/oh-my-zsh.sh ]; then
    export ZSH
    plugins=(
        git
        zsh-autosuggestions
        zsh-syntax-highlighting
    )
    source $ZSH/oh-my-zsh.sh
else
    # If Oh My Zsh is unavailable, let's set up the built-in zstyle matching regex
    zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
fi

# SDKMAN!
if [ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]; then
    export SDKMAN_DIR="${HOME}/.sdkman"
    source "$HOME/.sdkman/bin/sdkman-init.sh"
else
    printf "TODO\n" 1>&2
fi

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/zzapotheosis/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward

# Personal aliases
alias c > /dev/null 2>&1 || alias c='clear'
alias ls > /dev/null 2>&1 || alias ls="ls --color=auto"
alias l > /dev/null 2>&1 || alias l="ls -lah"
alias grep > /dev/null 2>&1 || alias grep="grep --color=auto"
alias edit > /dev/null 2>&1 || alias edit="${EDITOR}"
alias e > /dev/null 2>&1 || alias e="edit"
alias ec > /dev/null 2>&1 || alias ec="emacsclient -c"
alias ecnw > /dev/null 2>&1 || alias ecnw="emacsclient -c -nw"

# Set aliases if utilities/commands are available
command -v lsd > /dev/null 2>&1 && alias l='lsd -lah'
command -v rlwrap > /dev/null 2>&1 && command -v sbcl > /dev/null 2>&1 && alias sbcl='rlwrap sbcl'

# Starship!
command -v starship > /dev/null 2>&1 && eval "$(starship init zsh)"

