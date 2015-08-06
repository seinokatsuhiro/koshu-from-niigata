** -*- koshu -*-
**
**  INPUT
**    <stdin>
**    heading-gazou-calc.k
**
**  OUTPUT
**    <stdout>
**

|-- HEADING  /1 'ファイル名  /2 "実効画像幅[pixel]"  /3 "実効画像高さ[pixel]"  /4 "幅の解像度[dpi]"  /5 "高さの解像度[dpi]"  /6 'ビットの深さ  /7 '解像度の単位  /8 '色の表現  /9 "撮影日[YYYY-MM-DD]"  /10 '撮影時間  /11 '住所  /12 '地域  /13 'タイトル  /14 'ライセンス  /15 '分類1  /16 '分類2  /17 '分類3  /18 '備考1  /19 '備考2
|-- HEADING  /1 'ファイル名  /2 "実効画像幅[pixel]"  /3 "実効画像高さ[pixel]"  /4 "幅の解像度[dpi]"  /5 "高さの解像度[dpi]"  /6 'ビットの深さ  /7 '解像度の単位  /8 '色の表現  /9 "撮影日[YYYY/MM/DD]"  /10 '撮影時刻  /11 '住所  /12 '地域  /13 'タイトル  /14 'ライセンス  /15 '分類1  /16 '分類2  /17 '分類3  /18 '備考1  /19 '備考2
|-- HEADING  /1 'ファイル名  /2 "実効画像幅[pixel]"  /3 "実効画像高さ[pixel]"  /4 "幅の解像度[dpi]"  /5 "高さの解像度[dpi]"  /6 'ビットの深さ  /7 '解像度の単位  /8 '色の表現  /9 "撮影日[YYYY/MM/DD]"  /10 '撮影時刻  /11 '住所  /12 '地域  /13 '名称  /14 'ライセンス  /15 '分類1  /16 '分類2  /17 '分類3  /18 '備考1  /19 '備考2

*** 3 judges

|-- TERM  /pos 1  /n 1  /names { 'ファイル名 }
|-- TERM  /pos 2  /n 1  /names { "実効画像幅[pixel]" }
|-- TERM  /pos 3  /n 1  /names { "実効画像高さ[pixel]" }
|-- TERM  /pos 4  /n 1  /names { "幅の解像度[dpi]" }
|-- TERM  /pos 5  /n 1  /names { "高さの解像度[dpi]" }

|-- TERM  /pos 6  /n 1  /names { 'ビットの深さ }
|-- TERM  /pos 7  /n 1  /names { '解像度の単位 }
|-- TERM  /pos 8  /n 1  /names { '色の表現 }
|-- TERM  /pos 9  /n 2  /names { "撮影日[YYYY-MM-DD]" | "撮影日[YYYY/MM/DD]" }
|-- TERM  /pos 10  /n 2  /names { '撮影時刻 | '撮影時間 }

|-- TERM  /pos 11  /n 1  /names { '住所 }
|-- TERM  /pos 12  /n 1  /names { '地域 }
|-- TERM  /pos 13  /n 2  /names { 'タイトル | '名称 }
|-- TERM  /pos 14  /n 1  /names { 'ライセンス }
|-- TERM  /pos 15  /n 1  /names { '分類1 }

|-- TERM  /pos 16  /n 1  /names { '分類2 }
|-- TERM  /pos 17  /n 1  /names { '分類3 }
|-- TERM  /pos 18  /n 1  /names { '備考1 }
|-- TERM  /pos 19  /n 1  /names { '備考2 }

*** 19 judges

**
**  SUMMARY
**       3 judges on HEADING
**      19 judges on TERM
**      22 judges in total
**
