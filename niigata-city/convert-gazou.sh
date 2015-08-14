#!/bin/sh
#
#  概要
#    このスクリプトは、新潟市オープンデータのうち、
#    画像のメタデータを甲州記法に変換します。
#
#  使用法
#    ./convert-gazou.sh
#

. convert-sub.sh

# ********************************************  ファイル

convert_program=convert-gazou
  # このスクリプトの名前。

convert_root=www.city.niigata.lg.jp/shisei/seisaku/it/open-data/opendata-gazou
  # 画像系のオープンデータ DATA.csv の保存先。

convert_output_csv=$convert_output/gazou.csv
  # すべてのメタデータを書き出した中間ファイル。

convert_output_k=$convert_output/gazou.k
  # 甲州記法への変換先ファイル

convert_log=${convert_program}-log.k
  # 変換結果のログファイル。


# ********************************************  変換処理の本体

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

convert_prepare_dir $convert_output
convert_prepare_dir $convert_logdir

convert_body | convert_log_save

koshu-from-csv --license LICENSE --judge $convert_judge/gazou.judge \
    < $convert_output_csv > $convert_output_k

if [ ! $? = 0 ]; then
    echo "ABORT"
    exit 2
fi
