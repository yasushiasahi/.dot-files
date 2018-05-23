#!/usr/bin/zsh

\cd $_
if [ $? = 0 ]; then
    # echo 'succse'
    echo -e '\e[36m success \e[m'
    ls
fi
