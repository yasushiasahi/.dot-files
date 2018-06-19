# PATH >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# /usr/local/を優先に
PATH="/usr/local/bin:${PATH}"
export PATH

# anyenv
export PATH="$HOME/.anyenv/bin:$PATH"
eval "$(anyenv init -)"

# gtags
export GTAGSCONF="$HOME/.globalrc"
export GTAGSLABEL=pygments

export GOPATH="$HOME/.go"
export GOROOT="$HOME/.anyenv/envs/goenv/versions/1.10.3"
export PATH="$GOPATH/bin:$PATH"
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< PATH
