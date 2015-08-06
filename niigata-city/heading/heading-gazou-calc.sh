#!/bin/sh
#
#  画像メタデータの見出しの一覧を作成
#

NIIGATA_CITY_OD=../www.city.niigata.lg.jp/shisei/seisaku/it/open-data/

list_csv () {
    find $NIIGATA_CITY_OD/$1 -name '*.csv'
}

for csv in `list_csv opendata-gazou`; do
    head -1 $csv | koshu-from-csv
done | koshu -i heading-gazou-calc.k > heading-gazou-data.k
