# プロンプト
PROMPT='%m:%c %n$ '

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

# lsの色の設定 https://qiita.com/yuyuchu3333/items/84fa4e051c3325098be3
case ${OSTYPE} in
    darwin*) # macosの場合
        eval $(gdircolors ~/.dircolors-solarized)
        ;;
    linux*) # linuxの場合
        eval $(gdircolors ~/.dircolors-solarized)
        ;;
esac

# 補完候補のファイル、ディレクトリにlsと同様の色付け
if [ -n "$LS_COLORS" ]; then
    zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
fi

# alias
alias ls='gls -al --color=auto'




# zplug >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
case ${OSTYPE} in
    darwin*) # macosの場合
        export ZPLUG_HOME=/usr/local/opt/zplug
	source $ZPLUG_HOME/init.zsh
        ;;
    linux*) # linuxの場合
        source ~/.zplug/init.zsh
        ;;
esac
# theme
zplug "mafredri/zsh-async"
zplug "sindresorhus/pure"
# 構文のハイライト
zplug "zsh-users/zsh-syntax-highlighting"
# history関係
zplug "zsh-users/zsh-history-substring-search"
# タイプ補完
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "chrissicool/zsh-256color"
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
export PATH="/usr/local/opt/openssl/bin:$PATH"
