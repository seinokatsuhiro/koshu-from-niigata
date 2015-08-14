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

convert_program=convert.sh
  # このスクリプトの名前。

convert_root=www.city.niigata.lg.jp/shisei/seisaku/it/open-data
  # オープンデータ DATA.csv の保存先。
  # 新潟市のウェブサイトから wget-data.sh でダウンロードしたディレクトリ。
  # DATA.csv に含まれるアンダースコア (_) はハイフン (-) へ置き換えられます。

convert_judge=convert-judge
  # 各 DATA.csv に対応する判断の型 DATA.judge を格納するディレクトリ。

convert_output=convert-koshu
  # 変換結果 DATA.k の出力先ディレクトリ。

convert_log=convert-log.k
  # 変換結果のログファイル


# ********************************************  変換処理の本体

. convert-sub.sh

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

if [ ! -e $convert_output ]; then
    mkdir -p $convert_output
fi

convert_body | convert_log_save
