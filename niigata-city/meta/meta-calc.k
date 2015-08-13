-*- koshu -*-

=== license

  このデータセットは以下の著作物を改変して作成しています。
  新潟市オープンデータ、新潟市、クリエイティブ・コモンズ・ライセンス 表示 2.1 日本
  (CC-BY 2.1 JP) http://creativecommons.org/licenses/by/2.1/jp/

=== rel

|== META : meta
  --order --forward /data /name /content

meta : source META /data /name /content
  | hyphynate
  | subst /name ( if /name = "公表者（部局）" -> "公表者" : /name )

hyphynate : subst /data ( replace-all "_" "-" /data )

|== TERM : source TERM /data /name /type /note
  --order

