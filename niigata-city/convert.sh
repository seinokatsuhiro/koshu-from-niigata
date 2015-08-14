#!/bin/sh
#
#  概要
#    このスクリプトは、新潟市オープンデータを甲州記法に変換します。
#    変数 $convert_root の場所に CSV ファイルを含む
#    ディレクトリが保存された状態で実行してください。
#
#  使用法
#    ./convert.sh
#

. convert-sub.sh

# ********************************************  ファイル

convert_program=convert
  # このスクリプトの名前。

convert_root=www.city.niigata.lg.jp/shisei/seisaku/it/open-data
  # オープンデータ DATA.csv の保存先。
  # 新潟市のウェブサイトから wget-data.sh でダウンロードしたディレクトリ。
  # DATA.csv に含まれるアンダースコア (_) はハイフン (-) へ置き換えられます。


# ********************************************  変換処理の本体

convert_loop () {
    for loop_csv in `convert_csv_list | grep -v -f convert-ignore.txt`; do
        loop_base=`basename $loop_csv .csv | tr _ -`
        loop_koshu=$loop_base.k
        loop_judge=$convert_judge/$loop_base.judge

        if [ -e "$loop_judge" ]; then
            # convert
            convert_convert=`convert_incr $convert_convert`
            convert_koshu "$loop_judge" < $loop_csv > $convert_output/$loop_koshu

            if [ $? = 0 ]; then
                convert_log convert "$loop_csv"
            else
                echo "ABORT $loop_csv"
                exit 2
            fi

        else
            # skip
            convert_skip=`convert_incr $convert_skip`
            convert_log skip "$loop_csv"
        fi

    done
}


# ********************************************  メイン

convert_prepare_dir $convert_output
convert_prepare_dir $convert_logdir

convert_body | convert_log_save
