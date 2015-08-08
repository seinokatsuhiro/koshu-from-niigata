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


# ********************************************  ファイル

#  $convert_root に含まれる DATA.csv を対象として、
#  その CSV ファイルに対応する判断ファイル $convert_judge/DATA.judge を検索し、
#  断続ファイルがあれば、koshu-from-csv を使って、DATA.csv を甲州記法へ変換します。
#  変換結果は、$convert_output/DATA.k に出力されます。

convert_root=www.city.niigata.lg.jp/shisei/seisaku/it/open-data
  # オープンデータ DATA.csv の保存先。
  # 新潟市のウェブサイトから wget-data.sh でダウンロードしたディレクトリ。
  # DATA.csv に含まれるアンダースコア (_) はハイフン (-) へ置き換えられます。

convert_judge=convert-judge
  # 各 DATA.csv に対応する判断の型 DATA.judge を格納するディレクトリ。

convert_output=convert-koshu
  # 変換結果 DATA.k の出力先ディレクトリ。


# ********************************************  変換処理の本体

convert_body () {
    echo "** -*- koshu -*-"
    echo "**"
    echo "**  新潟市オープンデータを甲州記法へ変換したファイルの一覧です。"
    echo "**  このファイルは 'convert.sh' によって生成されました。"
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

    convert_convert=0
    convert_skip=0
    convert_loop
    convert_total=`expr $convert_convert + $convert_skip`

    echo
    echo "**"
    echo "**  件数"
    echo "**    convert  $convert_convert"
    echo "**    skip     $convert_skip"
    echo "**    合計     $convert_total"
    echo "**"
}

convert_loop () {
    for loop_csv in `convert_csv_list`; do
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

convert_csv_list () {
    find $convert_root -name '*.csv' | grep -v -f convert-ignore.txt
}

convert_incr () {
    expr $1 + 1
}

convert_koshu () {
    if [ -e "$1" ]; then
        koshu-from-csv --omit-first --header LICENSE `cat $1`
    else
        koshu-from-csv --omit-first --header LICENSE
    fi
}

convert_log () {
    convert_line=`wc -l < "$2"`
    convert_line=`echo $convert_line`
    echo "|-- CONVERT-LOG  /process '$1  /csv-lines $convert_line  /path \"$2\""
}


# ********************************************  メイン

if [ ! -e $convert_output ]; then
    mkdir $convert_output
fi

convert_cut_root () {
    sed "s:$convert_root/::"
}

convert_body | convert_cut_root | tee convert-log.k

