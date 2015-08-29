# I/O List

-  [koshu-from-csv ABC /a /b /c --input 123.csv](#koshu-from-csv-abc-a-b-c---input-123csv)
-  [koshu-from-csv --judge abc.judge --input 123.csv](#koshu-from-csv---judge-abcjudge---input-123csv)
-  [koshu-from-csv ABC /a /b /c --input 123.csv --number](#koshu-from-csv-abc-a-b-c---input-123csv---number)
-  [koshu-from-csv ABC /a /b /c --input 123h.csv --omit-first](#koshu-from-csv-abc-a-b-c---input-123hcsv---omit-first)
-  [koshu-from-csv ABC /a /b /c --input 123.csv --license DUMMY](#koshu-from-csv-abc-a-b-c---input-123csv---license-dummy)
-  [koshu-from-csv ABC /a /b /c --input 123.csv --header DUMMY](#koshu-from-csv-abc-a-b-c---input-123csv---header-dummy)



## koshu-from-csv ABC /a /b /c --input 123.csv

```
1,10,100
2,20,200
3,30,300
4,40,400
5,50,500
```

Command `koshu-from-csv ABC /a /b /c --input 123.csv` produces:

```
** -*- koshu -*-

|-- ABC  /a '1  /b '10  /c '100
|-- ABC  /a '2  /b '20  /c '200
|-- ABC  /a '3  /b '30  /c '300
|-- ABC  /a '4  /b '40  /c '400
|-- ABC  /a '5  /b '50  /c '500
```



## koshu-from-csv --judge abc.judge --input 123.csv

```
ABC
/a
/b
/c
```
```
1,10,100
2,20,200
3,30,300
4,40,400
5,50,500
```

Command `koshu-from-csv --judge abc.judge --input 123.csv` produces:

```
** -*- koshu -*-

|-- ABC  /a '1  /b '10  /c '100
|-- ABC  /a '2  /b '20  /c '200
|-- ABC  /a '3  /b '30  /c '300
|-- ABC  /a '4  /b '40  /c '400
|-- ABC  /a '5  /b '50  /c '500
```



## koshu-from-csv ABC /a /b /c --input 123.csv --number

```
1,10,100
2,20,200
3,30,300
4,40,400
5,50,500
```

Command `koshu-from-csv ABC /a /b /c --input 123.csv --number` produces:

```
** -*- koshu -*-

|-- ABC  /# 1  /a '1  /b '10  /c '100
|-- ABC  /# 2  /a '2  /b '20  /c '200
|-- ABC  /# 3  /a '3  /b '30  /c '300
|-- ABC  /# 4  /a '4  /b '40  /c '400
|-- ABC  /# 5  /a '5  /b '50  /c '500
```



## koshu-from-csv ABC /a /b /c --input 123h.csv --omit-first

```
a,b,c
1,10,100
2,20,200
3,30,300
4,40,400
5,50,500
```

Command `koshu-from-csv ABC /a /b /c --input 123h.csv --omit-first` produces:

```
** -*- koshu -*-

|-- ABC  /a '1  /b '10  /c '100
|-- ABC  /a '2  /b '20  /c '200
|-- ABC  /a '3  /b '30  /c '300
|-- ABC  /a '4  /b '40  /c '400
|-- ABC  /a '5  /b '50  /c '500
```



## koshu-from-csv ABC /a /b /c --input 123.csv --license DUMMY

```
1,10,100
2,20,200
3,30,300
4,40,400
5,50,500
```
```
Copyright (c) 2015, SEINO Katsuhiro

This is a dummy license file.
You can include this file with --license option.
The Koshu calculator copies license secitons
literally from input to output.

# Empty lines below are cut by converter.



```

Command `koshu-from-csv ABC /a /b /c --input 123.csv --license DUMMY` produces:

```
** -*- koshu -*-

=== license

  Copyright (c) 2015, SEINO Katsuhiro
  
  This is a dummy license file.
  You can include this file with --license option.
  The Koshu calculator copies license secitons
  literally from input to output.
  
  # Empty lines below are cut by converter.

=== rel

|-- ABC  /a '1  /b '10  /c '100
|-- ABC  /a '2  /b '20  /c '200
|-- ABC  /a '3  /b '30  /c '300
|-- ABC  /a '4  /b '40  /c '400
|-- ABC  /a '5  /b '50  /c '500
```



## koshu-from-csv ABC /a /b /c --input 123.csv --header DUMMY

```
1,10,100
2,20,200
3,30,300
4,40,400
5,50,500
```
```
Copyright (c) 2015, SEINO Katsuhiro

This is a dummy license file.
You can include this file with --license option.
The Koshu calculator copies license secitons
literally from input to output.

# Empty lines below are cut by converter.



```

Command `koshu-from-csv ABC /a /b /c --input 123.csv --header DUMMY` produces:

```
** -*- koshu -*-
**
**  Copyright (c) 2015, SEINO Katsuhiro
**
**  This is a dummy license file.
**  You can include this file with --license option.
**  The Koshu calculator copies license secitons
**  literally from input to output.
**
**  # Empty lines below are cut by converter.
**

|-- ABC  /a '1  /b '10  /c '100
|-- ABC  /a '2  /b '20  /c '200
|-- ABC  /a '3  /b '30  /c '300
|-- ABC  /a '4  /b '40  /c '400
|-- ABC  /a '5  /b '50  /c '500
```



## command

This document is produced by the command:

```
koshu-inout.sh -r -f cmd
```
