# I/O List

- [./from.sh](#fromsh)
- ./from.sh [box.csv](#boxcsv)
- ./from.sh [column.csv](#columncsv)
- ./from.sh [comma.csv](#commacsv)
- ./from.sh [empty.csv](#emptycsv)
- ./from.sh [line.csv](#linecsv)
- ./from.sh [square.csv](#squarecsv)
- ./from.sh [triangle.csv](#trianglecsv)



## [./from.sh](./from.sh)

```
#!/bin/sh
koshu-from-csv < $1
```



## [box.csv](box.csv)

```
aa,bb,cc
dd,,ee
ff,gg,hh
```

Command `./from.sh box.csv` produces:

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

Command `./from.sh column.csv` produces:

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

Command `./from.sh comma.csv` produces:

```
** -*- koshu -*-

|-- CSV  /1 ()  /2 ()
|-- CSV  /1 ()  /2 ()  /3 ()
|-- CSV  /1 ()  /2 ()  /3 ()  /4 ()
```



## [empty.csv](empty.csv)

```
```

Command `./from.sh empty.csv` produces:

```
** -*- koshu -*-

```



## [line.csv](line.csv)

```
aa,bb,cc
```

Command `./from.sh line.csv` produces:

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

Command `./from.sh square.csv` produces:

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

Command `./from.sh triangle.csv` produces:

```
** -*- koshu -*-

|-- CSV  /1 'aa  /2 'bb  /3 'cc
|-- CSV  /1 'dd  /2 'ee
|-- CSV  /1 'ff
```



## command

This document is produced by the command:

```
koshu-inout.sh -r -g -x csv ./from.sh
```
