#!/bin/sh
#
#  概要
#    このスクリプトは、新潟市オープンデータのうち、
#    年月単位-市区-年齢-男女-人口データを甲州記法に変換します。
#
#  使用法
#    ./convert-kujinkou.sh
#

. convert-sub.sh

# ********************************************  ファイル

convert_program=convert-kujinkou
  # このスクリプトの名前。

convert_root=www.city.niigata.lg.jp/shisei/seisaku/it/open-data/opendata-toukei/od-jinkou/od-jyukijinkou/od-kubetsujyuki.files
  # 人口のオープンデータ DATA.csv の保存先。

convert_output=convert-koshu/kujinkou
  # 変換結果 DATA.k の出力先ディレクトリ。

convert_log=${convert_program}-log.k
  # 変換結果のログファイル。


# ********************************************  変換処理の本体

convert_loop () {
    for loop_csv in `convert_csv_list`; do
        loop_base=`basename $loop_csv .csv | tr _ -`
        loop_koshu=$loop_base.k
        loop_judge=$convert_judge/kujinkou.judge

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
