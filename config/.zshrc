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

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=* l:|=*'

bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward

# Personal aliases
if [ -z "$(alias c       > /dev/null 2>&1)" ]; then alias c="clear"; fi
if [ -z "$(alias ls      > /dev/null 2>&1)" ]; then alias ls="ls --color=auto"; fi
if [ -z "$(alias l       > /dev/null 2>&1)" ]; then alias l="ls -lah"; fi
if [ -z "$(alias grep    > /dev/null 2>&1)" ]; then alias grep="grep --color=auto"; fi
if [ -z "$(alias edit    > /dev/null 2>&1)" ]; then alias edit="${EDITOR}"; fi
if [ -z "$(alias e       > /dev/null 2>&1)" ]; then alias e="edit"; fi
if [ -z "$(alias ec      > /dev/null 2>&1)" ]; then alias ec="emacsclient -c"; fi
if [ -z "$(alias ecnw    > /dev/null 2>&1)" ]; then alias ecnw="emacsclient -c -nw"; fi

# Update alias if lsd is installed
lsd --version &> /dev/null
if [ $? -eq 0 ]; then
    alias l="lsd -lah"
fi

# Starship!
#eval "$(starship init zsh)"
