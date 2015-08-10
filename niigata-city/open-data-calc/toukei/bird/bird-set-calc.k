-*- koshu -*-
**
**  説明
**    1. 野鳥が観察された回を、野鳥ごとに回の集合としてまとめます。
**    2. 佐潟のみ、鳥屋野潟のみ、両方で観察された野鳥に分類します。
**

input  "bird-watching-data.k"
output "bird-set-data.k"

|== 野鳥の観察回 : 野鳥の観察回
  --order --forward /場所 /種類

|== 野鳥の観察場所 : 野鳥
  | hier /種類 -to /g
  | add /場所 ( sort of to-set /g/場所 )
  | cut /g
  | hier /場所 -to /野鳥
  --table --order --forward /場所

野鳥の観察回 : 野鳥
  | hier /場所 /種類 -to /g
  | add /回 ( sort of to-set /g/回 )
  | cut /g

野鳥 : source 市民探鳥会-野鳥 /場所 /種類 /回
