# I/O List

- koshu-from-csv --input [box.csv](#boxcsv)
- koshu-from-csv --input [column.csv](#columncsv)
- koshu-from-csv --input [comma.csv](#commacsv)
- koshu-from-csv --input [empty.csv](#emptycsv)
- koshu-from-csv --input [line.csv](#linecsv)
- koshu-from-csv --input [square.csv](#squarecsv)
- koshu-from-csv --input [triangle.csv](#trianglecsv)



## [box.csv](box.csv)

```
aa,bb,cc
dd,,ee
ff,gg,hh
```

Command `koshu-from-csv --input box.csv` produces:

```
** -*- koshu -*-

|-- CSV  /1 'aa  /2 'bb  /3 'cc
|-- CSV  /1 'dd  /2 ()  /3 'ee
|-- CSV  /1 'ff  /2 'gg  /3 'hh
```



## [column.csv](column.csv)

```
aa
bb
cc
```

Command `koshu-from-csv --input column.csv` produces:

```
** -*- koshu -*-

|-- CSV  /1 'aa
|-- CSV  /1 'bb
|-- CSV  /1 'cc
```



## [comma.csv](comma.csv)

```
,
,,
,,,
```

Command `koshu-from-csv --input comma.csv` produces:

```
** -*- koshu -*-

|-- CSV  /1 ()  /2 ()
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 ()  /2 ()  /3 ()  /4 ()
```



## [empty.csv](empty.csv)

```
```

Command `koshu-from-csv --input empty.csv` produces:

```
** -*- koshu -*-

```



## [line.csv](line.csv)

```
aa,bb,cc
```

Command `koshu-from-csv --input line.csv` produces:

```
** -*- koshu -*-

|-- CSV  /1 'aa  /2 'bb  /3 'cc
```



## [square.csv](square.csv)

```
aa,bb,cc
dd,ee,ff
gg,hh,ii
```

Command `koshu-from-csv --input square.csv` produces:

```
** -*- koshu -*-

|-- CSV  /1 'aa  /2 'bb  /3 'cc
|-- CSV  /1 'dd  /2 'ee  /3 'ff
|-- CSV  /1 'gg  /2 'hh  /3 'ii
```



## [triangle.csv](triangle.csv)

```
aa,bb,cc
dd,ee
ff
```

Command `koshu-from-csv --input triangle.csv` produces:

```
** -*- koshu -*-

|-- CSV  /1 'aa  /2 'bb  /3 'cc
|-- CSV  /1 'dd  /2 'ee
|-- CSV  /1 'ff
```



## command

This document is produced by the command:

```
koshu-inout.sh -r -g -x csv koshu-from-csv --input
```
