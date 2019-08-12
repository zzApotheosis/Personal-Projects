# Go
if [ -d $HOME/go/bin ] ; then
    export GOPATH="$HOME/go/bin:$HOME/Documents/GitHub/Personal-Projects/Golang Practice"
fi

# Personal bin folder
if [ -d $HOME/bin ]; then
    export PATH="$PATH:$HOME/bin"
fi
