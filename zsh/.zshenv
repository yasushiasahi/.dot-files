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
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< PATH
