#!/bin/sh
#
#  概要
#    新潟市オープンデータのダウンロード
#
#  ログ
#    Total wall clock time: 55m 59s
#    Downloaded: 615 files, 7.7M in 45s (175 KB/s)
#

download_domain=http://www.city.niigata.lg.jp
download_dir=/shisei/seisaku/it/open-data/
download_url=$download_domain$download_dir
download_date=`date +%Y-%m-%d`

wget \
    --accept .html,.csv \
    --include-directories=$download_dir \
    --output-file=wget-$download_date.log \
    --quota=1000m \
    --random-wait \
    --recursive \
    --tries=10 \
    --wait=5 \
    $download_url
