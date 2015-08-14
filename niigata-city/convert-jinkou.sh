#!/bin/sh
#
#  概要
#    このスクリプトは、新潟市オープンデータのうち、
#    年月単位-市区-年齢-男女-人口データを甲州記法に変換します。
#
#  使用法
#    ./convert-jinkou.sh
#

. convert-sub.sh

# ********************************************  ファイル

convert_program=convert-jinkou
  # このスクリプトの名前。

convert_root=www.city.niigata.lg.jp/shisei/seisaku/it/open-data/opendata-toukei/od-jinkou/od-jyukijinkou/od-ku_danjo_nenrei
  # 人口のオープンデータ DATA.csv の保存先。

convert_output=convert-koshu/jinkou
  # 変換結果 DATA.k の出力先ディレクトリ。

convert_log=${convert_program}-log.k
  # 変換結果のログファイル。


# ********************************************  変換処理の本体

convert_loop () {
    convert_by $convert_judge/jinkou.judge
}


# ********************************************  メイン

convert_prepare_dir $convert_output
convert_prepare_dir $convert_logdir

convert_body | convert_log_save

