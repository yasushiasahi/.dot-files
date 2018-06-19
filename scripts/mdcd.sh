#!/usr/bin/zsh

if [ $2 =   ]; then
    new_directory_name=$1
else
    new_directory_name=$2
fi

mkdir $1 $2

if [ $? = 0 ]; then	                    # エラーが出なければ(直前の処理が [成功 $? = 0] [失敗 $? = 1])
    builtin cd ${new_directory_name}	    # 作成したディレクトリに素(buildin)のcdする。（cdだとエイリアスが呼ばれる）
    echo -e '\e[36m success \e[m'           # 色(36)を付けて'success'を出力
fi				            # if文終了
