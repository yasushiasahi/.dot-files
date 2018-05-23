#!/usr/bin/zsh

\cd $_				# cdの前に\を付けないとエイリアス名がcdなのでまた自分を呼び出して延々にループする
if [ $? = 0 ]; then
    echo -e '\e[36m success \e[m'
    ls
fi
