---
title: "Group with base, dplyr (tidyverse) & data.table"
output:
  html_document: 
    keep_md: yes
---


```r
library(data.table)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
pg <- fread("pigeon-racing.csv")
```



```r
aggregate(Ent ~ Breeder, pg, length)
```

```
##                    Breeder Ent
## 1                  4-Birds   4
## 2           7-11 Syndicate   2
## 3               A P C Loft  12
## 4              Alias-Alias  11
## 5              Alonso Loft   3
## 6           American Lofts   4
## 7              Andy Skwiat  10
## 8               B D P Loft   3
## 9         Baldwin & Tilson   7
## 10             Baysideboys   3
## 11             Bionic Loft   2
## 12         Braden/Olivieri   2
## 13         Bud & Orie Loft   2
## 14      Bynum Family Lofts   5
## 15           Captain Chuck  10
## 16          Centmeyer Loft   2
## 17   Charlie's Little Loft   3
## 18        Churn Creek Loft   3
## 19      Clear Springs Loft   3
## 20     Credeur Family Loft   2
## 21          Crestview Loft   4
## 22    Crowley's Ridge Loft   2
## 23      Cypress Hills Farm   2
## 24           Dal-Tex R P C   3
## 25             Dave Dudley   3
## 26            Dave Harrett   6
## 27            Debbie Ganus   6
## 28        Drama Queen Loft   3
## 29               Dunn Loft   1
## 30              E & E Loft   3
## 31           Emerson/Krass   5
## 32          Equalizer Loft   4
## 33             Family Loft  12
## 34      Fisher Family Loft   5
## 35            Flyhome Loft   2
## 36         Flying D Loft 2   3
## 37       Ganus Family Loft   6
## 38        Goshentown Lofts   4
## 39        Greek Connection   5
## 40            Greg Glazier   4
## 41       Hi-Cal Connection   5
## 42         Hutchins/Milner   5
## 43            Iv Aces Loft   2
## 44    J & M Partridge Loft   4
## 45                  Jb & D  13
## 46                  Jerdee   6
## 47       Jerry Allensworth   4
## 48           Jerry Johnson   2
## 49          Junior Juanich   6
## 50              Kiko Arana   4
## 51               King City   3
## 52             Kokomo Loft   2
## 53               Kwan Loft   4
## 54            Leyba's Loft   3
## 55       Little Reata Loft   6
## 56           Loizzi/Sikora   5
## 57           Lonestar Loft   4
## 58        Mayberry Classic   2
## 59       Mc Laughlin Lofts   4
## 60              Mike Belus   2
## 61           Miller Loft 2   1
## 62         Milner-Mckinsey   8
## 63              Mr. Dovely   1
## 64           N C Syndicate   6
## 65      Nanez Family Lofts   4
## 66         Non Pareil Loft   4
## 67             Oscar/Ralph   1
## 68                P J Loft   3
## 69            Protege Loft   4
## 70              Rayce Loft   4
## 71                  Redtex  12
## 72             Rick Barker   5
## 73 Sandstrom Brothers Loft   7
## 74      Shang N Dennis Yoh   5
## 75    Shawnie & Lenny Loft   4
## 76    Sierra Ranch Classic   6
## 77      Silver Dollar Loft   2
## 78         Skip's Janssens   5
## 79           Stelling Loft   4
## 80            Stenman Loft  10
## 81              T C R Loft   5
## 82        Terry Montgomery   2
## 83           Texas Outlaws   4
## 84         The Bailey Team   7
## 85     The Brand Syndicate   5
## 86               Tongol 11   3
## 87       Tony Family Lofts   3
## 88               Turk Loft   2
## 89                 Twin200   2
## 90          Woodsend/Perry   9
```

```r
table(pg$Breeder)
```

```
## 
##                 4-Birds          7-11 Syndicate              A P C Loft 
##                       4                       2                      12 
##             Alias-Alias             Alonso Loft          American Lofts 
##                      11                       3                       4 
##             Andy Skwiat              B D P Loft        Baldwin & Tilson 
##                      10                       3                       7 
##             Baysideboys             Bionic Loft         Braden/Olivieri 
##                       3                       2                       2 
##         Bud & Orie Loft      Bynum Family Lofts           Captain Chuck 
##                       2                       5                      10 
##          Centmeyer Loft   Charlie's Little Loft        Churn Creek Loft 
##                       2                       3                       3 
##      Clear Springs Loft     Credeur Family Loft          Crestview Loft 
##                       3                       2                       4 
##    Crowley's Ridge Loft      Cypress Hills Farm           Dal-Tex R P C 
##                       2                       2                       3 
##             Dave Dudley            Dave Harrett            Debbie Ganus 
##                       3                       6                       6 
##        Drama Queen Loft               Dunn Loft              E & E Loft 
##                       3                       1                       3 
##           Emerson/Krass          Equalizer Loft             Family Loft 
##                       5                       4                      12 
##      Fisher Family Loft            Flyhome Loft         Flying D Loft 2 
##                       5                       2                       3 
##       Ganus Family Loft        Goshentown Lofts        Greek Connection 
##                       6                       4                       5 
##            Greg Glazier       Hi-Cal Connection         Hutchins/Milner 
##                       4                       5                       5 
##            Iv Aces Loft    J & M Partridge Loft                  Jb & D 
##                       2                       4                      13 
##                  Jerdee       Jerry Allensworth           Jerry Johnson 
##                       6                       4                       2 
##          Junior Juanich              Kiko Arana               King City 
##                       6                       4                       3 
##             Kokomo Loft               Kwan Loft            Leyba's Loft 
##                       2                       4                       3 
##       Little Reata Loft           Loizzi/Sikora           Lonestar Loft 
##                       6                       5                       4 
##        Mayberry Classic       Mc Laughlin Lofts              Mike Belus 
##                       2                       4                       2 
##           Miller Loft 2         Milner-Mckinsey              Mr. Dovely 
##                       1                       8                       1 
##           N C Syndicate      Nanez Family Lofts         Non Pareil Loft 
##                       6                       4                       4 
##             Oscar/Ralph                P J Loft            Protege Loft 
##                       1                       3                       4 
##              Rayce Loft                  Redtex             Rick Barker 
##                       4                      12                       5 
## Sandstrom Brothers Loft      Shang N Dennis Yoh    Shawnie & Lenny Loft 
##                       7                       5                       4 
##    Sierra Ranch Classic      Silver Dollar Loft         Skip's Janssens 
##                       6                       2                       5 
##           Stelling Loft            Stenman Loft              T C R Loft 
##                       4                      10                       5 
##        Terry Montgomery           Texas Outlaws         The Bailey Team 
##                       2                       4                       7 
##     The Brand Syndicate               Tongol 11       Tony Family Lofts 
##                       5                       3                       3 
##               Turk Loft                 Twin200          Woodsend/Perry 
##                       2                       2                       9
```

```r
data.frame(table(pg$Breeder))
```

```
##                       Var1 Freq
## 1                  4-Birds    4
## 2           7-11 Syndicate    2
## 3               A P C Loft   12
## 4              Alias-Alias   11
## 5              Alonso Loft    3
## 6           American Lofts    4
## 7              Andy Skwiat   10
## 8               B D P Loft    3
## 9         Baldwin & Tilson    7
## 10             Baysideboys    3
## 11             Bionic Loft    2
## 12         Braden/Olivieri    2
## 13         Bud & Orie Loft    2
## 14      Bynum Family Lofts    5
## 15           Captain Chuck   10
## 16          Centmeyer Loft    2
## 17   Charlie's Little Loft    3
## 18        Churn Creek Loft    3
## 19      Clear Springs Loft    3
## 20     Credeur Family Loft    2
## 21          Crestview Loft    4
## 22    Crowley's Ridge Loft    2
## 23      Cypress Hills Farm    2
## 24           Dal-Tex R P C    3
## 25             Dave Dudley    3
## 26            Dave Harrett    6
## 27            Debbie Ganus    6
## 28        Drama Queen Loft    3
## 29               Dunn Loft    1
## 30              E & E Loft    3
## 31           Emerson/Krass    5
## 32          Equalizer Loft    4
## 33             Family Loft   12
## 34      Fisher Family Loft    5
## 35            Flyhome Loft    2
## 36         Flying D Loft 2    3
## 37       Ganus Family Loft    6
## 38        Goshentown Lofts    4
## 39        Greek Connection    5
## 40            Greg Glazier    4
## 41       Hi-Cal Connection    5
## 42         Hutchins/Milner    5
## 43            Iv Aces Loft    2
## 44    J & M Partridge Loft    4
## 45                  Jb & D   13
## 46                  Jerdee    6
## 47       Jerry Allensworth    4
## 48           Jerry Johnson    2
## 49          Junior Juanich    6
## 50              Kiko Arana    4
## 51               King City    3
## 52             Kokomo Loft    2
## 53               Kwan Loft    4
## 54            Leyba's Loft    3
## 55       Little Reata Loft    6
## 56           Loizzi/Sikora    5
## 57           Lonestar Loft    4
## 58        Mayberry Classic    2
## 59       Mc Laughlin Lofts    4
## 60              Mike Belus    2
## 61           Miller Loft 2    1
## 62         Milner-Mckinsey    8
## 63              Mr. Dovely    1
## 64           N C Syndicate    6
## 65      Nanez Family Lofts    4
## 66         Non Pareil Loft    4
## 67             Oscar/Ralph    1
## 68                P J Loft    3
## 69            Protege Loft    4
## 70              Rayce Loft    4
## 71                  Redtex   12
## 72             Rick Barker    5
## 73 Sandstrom Brothers Loft    7
## 74      Shang N Dennis Yoh    5
## 75    Shawnie & Lenny Loft    4
## 76    Sierra Ranch Classic    6
## 77      Silver Dollar Loft    2
## 78         Skip's Janssens    5
## 79           Stelling Loft    4
## 80            Stenman Loft   10
## 81              T C R Loft    5
## 82        Terry Montgomery    2
## 83           Texas Outlaws    4
## 84         The Bailey Team    7
## 85     The Brand Syndicate    5
## 86               Tongol 11    3
## 87       Tony Family Lofts    3
## 88               Turk Loft    2
## 89                 Twin200    2
## 90          Woodsend/Perry    9
```



```r
count(pg, Breeder)
```

```
## # A tibble: 90 x 2
##    Breeder              n
##    <chr>            <int>
##  1 4-Birds              4
##  2 7-11 Syndicate       2
##  3 A P C Loft          12
##  4 Alias-Alias         11
##  5 Alonso Loft          3
##  6 American Lofts       4
##  7 Andy Skwiat         10
##  8 B D P Loft           3
##  9 Baldwin & Tilson     7
## 10 Baysideboys          3
## # … with 80 more rows
```

```r
#or with magrittr piping
pg %>% count(Breeder)
```

```
## # A tibble: 90 x 2
##    Breeder              n
##    <chr>            <int>
##  1 4-Birds              4
##  2 7-11 Syndicate       2
##  3 A P C Loft          12
##  4 Alias-Alias         11
##  5 Alonso Loft          3
##  6 American Lofts       4
##  7 Andy Skwiat         10
##  8 B D P Loft           3
##  9 Baldwin & Tilson     7
## 10 Baysideboys          3
## # … with 80 more rows
```



```r
#dt[filter, select, group]
pg[, .N, Breeder]
```

```
##                     Breeder  N
##  1:           Texas Outlaws  4
##  2:          Junior Juanich  6
##  3:       Jerry Allensworth  4
##  4:             Alias-Alias 11
##  5:            Greg Glazier  4
##  6:           Dal-Tex R P C  3
##  7:           N C Syndicate  6
##  8:        Baldwin & Tilson  7
##  9:                  Redtex 12
## 10:        Terry Montgomery  2
## 11:          Woodsend/Perry  9
## 12:          Crestview Loft  4
## 13:        Goshentown Lofts  4
## 14:            Flyhome Loft  2
## 15:         Skip's Janssens  5
## 16:         Bud & Orie Loft  2
## 17:         Milner-Mckinsey  8
## 18:    Shawnie & Lenny Loft  4
## 19:      Nanez Family Lofts  4
## 20:                  Jb & D 13
## 21:      Silver Dollar Loft  2
## 22:            Protege Loft  4
## 23:              Mike Belus  2
## 24:    Sierra Ranch Classic  6
## 25:              A P C Loft 12
## 26:        Churn Creek Loft  3
## 27: Sandstrom Brothers Loft  7
## 28:         Flying D Loft 2  3
## 29:      Clear Springs Loft  3
## 30:          Equalizer Loft  4
## 31:            Stenman Loft 10
## 32:    Crowley's Ridge Loft  2
## 33:            Dave Harrett  6
## 34:             Dave Dudley  3
## 35:       Mc Laughlin Lofts  4
## 36:       Little Reata Loft  6
## 37:           Emerson/Krass  5
## 38:         Non Pareil Loft  4
## 39:             Andy Skwiat 10
## 40:          American Lofts  4
## 41:             Alonso Loft  3
## 42:           Captain Chuck 10
## 43:      Shang N Dennis Yoh  5
## 44:    J & M Partridge Loft  4
## 45:              B D P Loft  3
## 46:              T C R Loft  5
## 47:             Family Loft 12
## 48:   Charlie's Little Loft  3
## 49:             Oscar/Ralph  1
## 50:         The Bailey Team  7
## 51:               Kwan Loft  4
## 52:       Ganus Family Loft  6
## 53:                 4-Birds  4
## 54:        Greek Connection  5
## 55:               Turk Loft  2
## 56:            Leyba's Loft  3
## 57:           Lonestar Loft  4
## 58:      Bynum Family Lofts  5
## 59:     The Brand Syndicate  5
## 60:             Kokomo Loft  2
## 61:           Loizzi/Sikora  5
## 62:              Rayce Loft  4
## 63:             Bionic Loft  2
## 64:            Iv Aces Loft  2
## 65:      Fisher Family Loft  5
## 66:     Credeur Family Loft  2
## 67:              Kiko Arana  4
## 68:             Rick Barker  5
## 69:            Debbie Ganus  6
## 70:                P J Loft  3
## 71:               King City  3
## 72:          Centmeyer Loft  2
## 73:       Hi-Cal Connection  5
## 74:           Stelling Loft  4
## 75:               Tongol 11  3
## 76:               Dunn Loft  1
## 77:          7-11 Syndicate  2
## 78:        Drama Queen Loft  3
## 79:         Hutchins/Milner  5
## 80:           Miller Loft 2  1
## 81:              Mr. Dovely  1
## 82:      Cypress Hills Farm  2
## 83:           Jerry Johnson  2
## 84:                 Twin200  2
## 85:             Baysideboys  3
## 86:        Mayberry Classic  2
## 87:       Tony Family Lofts  3
## 88:                  Jerdee  6
## 89:              E & E Loft  3
## 90:         Braden/Olivieri  2
##                     Breeder  N
```


```r
pg[Color == "BBWF", .(Sex, Speed), .(Breeder)][, .N, .(Breeder)][order(-N)]
```

```
##                     Breeder N
##  1:              Mike Belus 2
##  2:             Andy Skwiat 2
##  3:       Ganus Family Loft 2
##  4:          American Lofts 2
##  5:             Family Loft 2
##  6:      Shang N Dennis Yoh 2
##  7:       Little Reata Loft 2
##  8:            Greg Glazier 1
##  9:         Milner-Mckinsey 1
## 10:      Nanez Family Lofts 1
## 11:    Sierra Ranch Classic 1
## 12:         Skip's Janssens 1
## 13:              A P C Loft 1
## 14:       Jerry Allensworth 1
## 15:            Leyba's Loft 1
## 16:             Alias-Alias 1
## 17:              Kiko Arana 1
## 18:              Rayce Loft 1
## 19:           Emerson/Krass 1
## 20:       Hi-Cal Connection 1
## 21:          Centmeyer Loft 1
## 22:          7-11 Syndicate 1
## 23:               Tongol 11 1
## 24:             Alonso Loft 1
## 25:            Debbie Ganus 1
## 26:                  Jerdee 1
## 27:         Hutchins/Milner 1
## 28: Sandstrom Brothers Loft 1
## 29:         Braden/Olivieri 1
##                     Breeder N
```
