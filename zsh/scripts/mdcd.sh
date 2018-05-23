#!/usr/bin/zsh

option=''				    # 変数optionを''として初期化
new_directory_name=$_		            # 変数'new_directory_name'にコマンドの最後の引数を代入

if [ $1 = '-p' ]; then		            # $1(第一引数)が'-p'なら
    option='-p'			            # 変数opstionの中身を'-p'に変える)
fi				            # if文終了

mkdir ${option} ${new_directory_name}	    # ディレクトリを作成 第一引数がならpオプションが付く

if [ $? = 0 ]; then	                    # エラーが出なければ(直前の処理が [成功 $? = 0] [失敗 $? = 1])
    builtin cd ${new_directory_name}	    # 作成したディレクトリに素(buildin)のcdする。（cdだとエイリアスが呼ばれる）
    echo -e '\e[36m success \e[m'           # 色(36)を付けて'success'を出力
fi				            # if文終了
