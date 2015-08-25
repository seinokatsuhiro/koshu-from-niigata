#!/bin/sh
#
#  概要
#    README.md を作成します．
#
#  使用法
#    ./convert-readme.sh
#

od_url=http://www.city.niigata.lg.jp/shisei/seisaku/it/open-data/

md () {
    md_head
    md_list toukei "統計"
    md_list tetsuduki "行政手続き"
    md_list gis "GIS (地理)"
    md_trailer
}

md_head () {
    echo "# 新潟市オープンデータ"
    echo
    echo "[新潟市のオープンデータ]($od_url)"
    echo "を甲州記法に変換したデータを一覧しています。"
    echo
}

md_trailer () {
    echo 'この一覧は `convert-readme.sh` を実行して再作成してください。'
    echo
}

md_list () {
    echo "### $2"
    echo
    echo "| 種類 | ファイル | 項目数 | データ数 |"
    echo "|------|----------|-------:|---------:|"
    for j in `find_judge $1`; do
        b=`basename $j .judge`
        k=`echo $j | change_dir | change_ext`
        echo "| [`data_title $j`]($k) | $b | `data_width $j` | `data_count $k` |"
    done
    echo
}

find_judge () {
    ls convert-judge/$1/*.judge
}

change_dir () {
    sed 's/^convert-judge/open-data/'
}

change_ext () {
    sed 's/[.]judge$/.k/'
}

data_title () {
    head -1 "$1"
}

data_width () {
    grep "^/" "$1" | line_count
}

data_count () {
    grep "^[|]--" "$1" | line_count
}

line_count () {
    wc -l | sed 's/^ *//'
}

md | tee README.md
