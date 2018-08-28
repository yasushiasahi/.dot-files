# Golang
export GOROOT=`go env GOROOT`
export GOPATH="$HOME/dev"
export PATH="$PATH:$GOPATH/bin"

# postgres
export PATH="$PATH:/Applications/Postgres.app/Contents/Versions/9.6/bin"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/asahi/Desktop/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/asahi/Desktop/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/asahi/Desktop/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/asahi/Desktop/google-cloud-sdk/completion.zsh.inc'; fi
