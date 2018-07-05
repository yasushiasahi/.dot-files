# PATH >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# anyenv
export PATH="$HOME/.anyenv/bin:$PATH"
eval "$(anyenv init -)"

# /usr/local/を優先に
export PATH="/usr/local/bin:$PATH"

# export PATH="/usr/local/opt/openssl/bin:$PATH" なぜ設定してたのか解らない。
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< PATH


# 日本語を使用
export LANG=ja_JP.UTF-8

# emacs風キーバインド
bindkey -e

# history
HISTFILE=~/.zsh_hist
HISTSIZE=10000
SAVEHIST=10000
setopt extended_history #ヒストリに実行時間も保存
setopt hist_ignore_dups #直前と同じコマンドはヒストリに追加しない

# シェルのプロセスごとに履歴を共有
setopt share_history

# N P による履歴呼び出しを入力済み文字にマッチしたコマンド検索にする
bindkey '^P' history-beginning-search-backward
bindkey '^N' history-beginning-search-forward

# 自動補完を有効にする
# コマンドの引数やパス名を途中まで入力して <Tab> を押すといい感じに補完してくれる
# 例： `cd path/to/<Tab>`, `ls -<Tab>`
autoload -U compinit; compinit
compinit
zstyle ':completion:*:default' menu select=1

# 入力したコマンドが存在せず、かつディレクトリ名と一致するなら、ディレクトリに cd する
# 例： /usr/bin と入力すると /usr/bin ディレクトリに移動
setopt auto_cd

# 256色を有効にする
export TERM=xterm-256color

# emacsを標準エディターにする
export EDITOR=emacs

# gtags
export GTAGSCONF="$HOME/.dot-files/etc/.globalrc"
export GTAGSLABEL=pygments

# 補完候補のファイル、ディレクトリにlsと同様の色付け
if [ -n "$LS_COLORS" ]; then
    zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
fi


bindkey '^]' peco-src

function peco-src() {
    local src=$(ghq list --full-path | peco --query "$LBUFFER")
    if [-n "$src"]; then
	BUFFER="cd $src"
	zle accept-line
    fi
    zle -R -c
}
zle -N peco-src

# alias >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
scripts_path=~/.dot-files/scripts

# lsの色の設定 https://qiita.com/yuyuchu3333/items/84fa4e051c3325098be3
case ${OSTYPE} in
    darwin*) # macosの場合
        eval $(gdircolors $HOME/.dot-files/etc/dircolors.ansi-dark)
	alias ls='gls -AlXhF --color=auto'
        ;;
    linux*) # linuxの場合
        eval $(dircolors $HOME/.dot-files/etc/dircolors.ansi-dark)
	alias ls='ls -AlXhF --color=auto'
        ;;
esac

alias tree='tree -C' # treeに色を付ける
alias diff='colordiff' # 色付きのdiffを使用
alias cd='source ${scripts_path}/cdls.sh' # cdした後に自動でlsする
alias mdcd='source ${scripts_path}/mdcd.sh' # mdcd hoge でhogeディレクトリを作って移動する

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< alias


# zplug >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
source ~/.zplug/init.zsh
# theme
zplug "mafredri/zsh-async"
zplug "sindresorhus/pure"
# history関係
zplug "zsh-users/zsh-history-substring-search"
# タイプ補完
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "chrissicool/zsh-256color"
# 構文のハイライト
zplug "zsh-users/zsh-syntax-highlighting", defer:2
# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  fi
fi
# Then, source plugins and add commands to $PATH
zplug load --verbose
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< zplug


# theme pure の設定
autoload -U promptinit; promptinit
prompt pure

# zsh-autosuggestionsの文字色を変更 default='fg=8'
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=23'
