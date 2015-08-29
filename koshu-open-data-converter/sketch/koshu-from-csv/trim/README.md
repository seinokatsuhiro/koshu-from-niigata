# I/O List

-  [koshu-from-csv --input trim.csv](#koshu-from-csv---input-trimcsv)
-  [koshu-from-csv --input trim.csv --trim](#koshu-from-csv---input-trimcsv---trim)
-  [koshu-from-csv --input trim.csv --trim-both](#koshu-from-csv---input-trimcsv---trim-both)
-  [koshu-from-csv --input trim.csv --trim-left](#koshu-from-csv---input-trimcsv---trim-left)
-  [koshu-from-csv --input trim.csv --trim-right](#koshu-from-csv---input-trimcsv---trim-right)



## koshu-from-csv --input trim.csv

```
aa,bb,cc
dd ,ee  ,ff   
 gg,  hh,   ii
,,
, ,
 ,,
 A A ,	BB	,CC
　ああ　,　　いい,うう
```

Command `koshu-from-csv --input trim.csv` produces:

```
** -*- koshu -*-

|-- CSV  /1 'aa  /2 'bb  /3 'cc
|-- CSV  /1 "dd "  /2 "ee  "  /3 "ff   "
|-- CSV  /1 " gg"  /2 "  hh"  /3 "   ii"
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 ()  /2 " "  /3 ()
|-- CSV  /1 " "  /2 ()  /3 ()
|-- CSV  /1 " A A "  /2 <tab> "BB" <tab>  /3 'CC
|-- CSV  /1 "　ああ　"  /2 "　　いい"  /3 'うう
```



## koshu-from-csv --input trim.csv --trim

```
aa,bb,cc
dd ,ee  ,ff   
 gg,  hh,   ii
,,
, ,
 ,,
 A A ,	BB	,CC
　ああ　,　　いい,うう
```

Command `koshu-from-csv --input trim.csv --trim` produces:

```
** -*- koshu -*-

|-- CSV  /1 'aa  /2 'bb  /3 'cc
|-- CSV  /1 'dd  /2 'ee  /3 'ff
|-- CSV  /1 'gg  /2 'hh  /3 'ii
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 "A A"  /2 'BB  /3 'CC
|-- CSV  /1 'ああ  /2 'いい  /3 'うう
```



## koshu-from-csv --input trim.csv --trim-both

```
aa,bb,cc
dd ,ee  ,ff   
 gg,  hh,   ii
,,
, ,
 ,,
 A A ,	BB	,CC
　ああ　,　　いい,うう
```

Command `koshu-from-csv --input trim.csv --trim-both` produces:

```
** -*- koshu -*-

|-- CSV  /1 'aa  /2 'bb  /3 'cc
|-- CSV  /1 'dd  /2 'ee  /3 'ff
|-- CSV  /1 'gg  /2 'hh  /3 'ii
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 "A A"  /2 'BB  /3 'CC
|-- CSV  /1 'ああ  /2 'いい  /3 'うう
```



## koshu-from-csv --input trim.csv --trim-left

```
aa,bb,cc
dd ,ee  ,ff   
 gg,  hh,   ii
,,
, ,
 ,,
 A A ,	BB	,CC
　ああ　,　　いい,うう
```

Command `koshu-from-csv --input trim.csv --trim-left` produces:

```
** -*- koshu -*-

|-- CSV  /1 'aa  /2 'bb  /3 'cc
|-- CSV  /1 "dd "  /2 "ee  "  /3 "ff   "
|-- CSV  /1 'gg  /2 'hh  /3 'ii
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 "A A "  /2 "BB" <tab>  /3 'CC
|-- CSV  /1 "ああ　"  /2 'いい  /3 'うう
```



## koshu-from-csv --input trim.csv --trim-right

```
aa,bb,cc
dd ,ee  ,ff   
 gg,  hh,   ii
,,
, ,
 ,,
 A A ,	BB	,CC
　ああ　,　　いい,うう
```

Command `koshu-from-csv --input trim.csv --trim-right` produces:

```
** -*- koshu -*-

|-- CSV  /1 'aa  /2 'bb  /3 'cc
|-- CSV  /1 'dd  /2 'ee  /3 'ff
|-- CSV  /1 " gg"  /2 "  hh"  /3 "   ii"
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 " A A"  /2 <tab> "BB"  /3 'CC
|-- CSV  /1 "　ああ"  /2 "　　いい"  /3 'うう
```



## command

This document is produced by the command:

```
koshu-inout.sh -r -f cmd
```
