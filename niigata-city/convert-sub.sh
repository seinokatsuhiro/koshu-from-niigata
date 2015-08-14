#!/bin/sh
#
#  概要
#    新潟市オープンデータの甲州記法化のためのサブルーチン
#
#  使用法
#    このシェル・スクリプトを読み込み、convert_loop を定義した上で、
#    convert_body を実行してください。
#


# ********************************************  変数

#  $convert_root に含まれる DATA.csv を対象として、
#  その CSV ファイルに対応する判断ファイル $convert_judge/DATA.judge を検索し、
#  断続ファイルがあれば、koshu-from-csv を使って、DATA.csv を甲州記法へ変換します。
#  変換結果は、$convert_output/DATA.k に出力されます。

convert_program=UNKNOWN
  # このスクリプトの名前。

convert_root=www.city.niigata.lg.jp/shisei/seisaku/it/open-data
  # オープンデータ DATA.csv の保存先。
  # 新潟市のウェブサイトから wget-data.sh でダウンロードしたディレクトリ。
  # DATA.csv に含まれるアンダースコア (_) はハイフン (-) へ置き換えられます。

convert_judge=convert-judge
  # 各 DATA.csv に対応する判断の型 DATA.judge を格納するディレクトリ。

convert_output=convert-koshu
  # 変換結果 DATA.k の出力先ディレクトリ。

convert_logdir=convert-log
  # 変換結果のログデータの保存先ディレクトリ。

convert_log=convert-log.k
  # 変換結果のログファイル。


# ********************************************  サブルーチン

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
    echo "**  このファイルは '$convert_program.sh' によって生成されました。"
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

convert_by () {
    for loop_csv in `convert_csv_list`; do
        loop_base=`basename $loop_csv .csv | tr _ -`
        loop_koshu=$loop_base.k
        loop_judge=$1

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

convert_log () {
    convert_line=`wc -l < "$2"`
    convert_line=`echo $convert_line`
    echo "|-- CONVERT-LOG  /process '$1  /csv-lines $convert_line  /path \"$2\""
}

convert_log_save () {
    convert_cut_root | tee convert-log/$convert_log
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

convert_prepare_dir () {
    if [ ! -e "$1" ]; then
        mkdir -p "$1"
    fi
}
