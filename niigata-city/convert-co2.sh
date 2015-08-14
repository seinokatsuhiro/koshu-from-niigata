#!/bin/sh
#
#  概要
#    このスクリプトは、新潟市オープンデータのうち、
#    二酸化炭素排出量データを甲州記法に変換します。
#
#  使用法
#    ./convert-co2.sh
#

. convert-sub.sh

# ********************************************  ファイル

convert_program=convert-co2
  # このスクリプトの名前。

convert_root=www.city.niigata.lg.jp/shisei/seisaku/it/open-data/opendata-toukei/od-kankyo/od-ondanka/od-co2.files
  # オープンデータ DATA.csv の保存先。

convert_output=convert-koshu/co2
  # 変換結果 DATA.k の出力先ディレクトリ。

convert_log=${convert_program}-log.k
  # 変換結果のログファイル。


# ********************************************  変換処理の本体

convert_loop () {
    convert_by $convert_judge/co2.judge
}


# ********************************************  メイン

convert_prepare_dir $convert_output
convert_prepare_dir $convert_logdir

convert_body | convert_log_save

