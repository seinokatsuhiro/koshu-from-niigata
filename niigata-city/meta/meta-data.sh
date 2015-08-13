#!/bin/sh

od_root=../www.city.niigata.lg.jp/shisei/seisaku/it/open-data

list_html () {
    find $od_root -name '*.html'
}

for html in `list_html`; do
    niigata-city-meta-data $html
done > meta-temp.k

koshu -i meta-calc.k < meta-temp.k > META.k
