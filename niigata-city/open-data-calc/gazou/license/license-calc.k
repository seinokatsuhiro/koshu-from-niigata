** -*- koshu -*-
**
**  画像データのライセンスを一覧します。
**

input  "../../../open-data/gazou/gazou.k"
output "license-data.k"

|== ライセンス : source 画像 /ファイル名 /ライセンス
  | hier /ライセンス -to /g
  | add /データ数 ( length /g/ファイル名 )
  | cut /g
  --order --forward /ライセンス
