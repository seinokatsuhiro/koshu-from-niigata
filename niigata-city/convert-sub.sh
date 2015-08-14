#!/bin/sh
#
#  概要
#    新潟市オープンデータの甲州記法化のためのサブルーチン
#
#  使用法
#    このシェル・スクリプトを読み込み、convert_loop を定義した上で、
#    convert_body を実行してください。
#

convert_body () {
    convert_convert=0
    convert_skip=0
    convert_head
    convert_loop

    convert_total=`expr $convert_convert + $convert_skip`
    convert_summary
}

convert_head () {
    echo "** -*- koshu -*-"
    echo "**"
    echo "**  新潟市オープンデータを甲州記法へ変換したファイルの一覧です。"
    echo "**  このファイルは '$convert_program' によって生成されました。"
    echo "**"
    echo
    echo "=== license"
    echo
    echo "  このデータセットは以下の著作物を改変して作成しています。"
    echo "  新潟市オープンデータ、新潟市、クリエイティブ・コモンズ・ライセンス 表示 2.1 日本"
    echo "  (CC-BY 2.1 JP) http://creativecommons.org/licenses/by/2.1/jp/"
    echo
    echo "=== rel"
    echo
}

convert_summary () {
    echo
    echo "**"
    echo "**  件数"
    echo "**    convert  $convert_convert"
    echo "**    skip     $convert_skip"
    echo "**    合計     $convert_total"
    echo "**"
}

convert_log () {
    convert_line=`wc -l < "$2"`
    convert_line=`echo $convert_line`
    echo "|-- CONVERT-LOG  /process '$1  /csv-lines $convert_line  /path \"$2\""
}

convert_log_save () {
    convert_cut_root | tee $convert_log
}

convert_csv_list () {
    find $convert_root -name '*.csv'
}

convert_cut_root () {
    sed "s:$convert_root/::"
}

convert_incr () {
    expr $1 + 1
}

convert_koshu () {
    if [ -e "$1" ]; then
        koshu-from-csv --omit-first --license LICENSE --judge "$1"
    else
        koshu-from-csv --omit-first --license LICENSE
    fi
}

