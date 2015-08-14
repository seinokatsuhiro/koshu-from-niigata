#!/bin/sh
#
#  概要
#    このスクリプトは、新潟市オープンデータのうち、
#    画像のメタデータを甲州記法に変換します。
#
#  使用法
#    ./convert-gazou.sh
#


# ********************************************  ファイル

convert_program=convert-gazou.sh
  # このスクリプトの名前。

convert_root=www.city.niigata.lg.jp/shisei/seisaku/it/open-data/opendata-gazou
  # 画像系のオープンデータ DATA.csv の保存先。

convert_judge=convert-judge
  # 各 DATA.csv に対応する判断の型 DATA.judge を格納するディレクトリ。

convert_output=convert-koshu
  # 変換結果 DATA.k の出力先ディレクトリ。

convert_output_csv=$convert_output/gazou.csv
  # すべてのメタデータを書き出した中間ファイル。

convert_output_k=$convert_output/gazou.k
  # 甲州記法への変換先ファイル


# ********************************************  変換処理の本体

. convert-sub.sh

convert_loop () {
    if [ -e $convert_output_csv ]; then
        rm $convert_output_csv
    fi

    for loop_csv in `convert_csv_list`; do
        convert_convert=`convert_incr $convert_convert`
        tail -1 $loop_csv >> $convert_output_csv
        convert_log convert "$loop_csv"
    done
}


# ********************************************  メイン

if [ ! -e $convert_output ]; then
    mkdir -p $convert_output
fi

convert_body | convert_cut_root | tee convert-gazou-log.k

koshu-from-csv --license LICENSE --judge $convert_judge/gazou.judge \
    < $convert_output_csv > $convert_output_k

if [ ! $? = 0 ]; then
    echo "ABORT"
    exit 2
fi
