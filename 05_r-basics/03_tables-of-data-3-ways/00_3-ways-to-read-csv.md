---
title: "Read .csv files with base, readr (tidyverse) & data.table"
output: 
  html_document: 
    keep_md: yes
---



# base

```r
pigeon_racing_df <- read.csv("pigeon-racing.csv", stringsAsFactors = FALSE)
system.time(read.csv("ticket_cleint.csv", stringsAsFactors = FALSE))
```

```
##    user  system elapsed 
##   9.414   0.302   9.848
```

# readr (need to assign to new data.frame)

```r
library(readr)
pigeon_racing_tbl <- read_csv("pigeon-racing.csv", col_types = "c")
```

```
## Warning: Unnamed `col_types` should have the same length as `col_names`.
## Using smaller of the two.
```

```
## Warning in rbind(names(probs), probs_f): number of columns of result is not
## a multiple of vector length (arg 1)
```

```
## Warning: 400 parsing failures.
## row # A tibble: 5 x 5 col     row col   expected  actual     file                expected   <int> <chr> <chr>     <chr>      <chr>               actual 1     1 <NA>  1 columns 11 columns 'pigeon-racing.csv' file 2     2 <NA>  1 columns 11 columns 'pigeon-racing.csv' row 3     3 <NA>  1 columns 11 columns 'pigeon-racing.csv' col 4     4 <NA>  1 columns 11 columns 'pigeon-racing.csv' expected 5     5 <NA>  1 columns 11 columns 'pigeon-racing.csv'
## ... ................. ... ...................................................... ........ ...................................................... ...... ...................................................... .... ...................................................... ... ...................................................... ... ...................................................... ........ ......................................................
## See problems(...) for more details.
```

```r
system.time(read_csv("ticket_cleint.csv"))
```

```
## Parsed with column specification:
## cols(
##   .default = col_character(),
##   num_client = col_integer(),
##   age = col_integer(),
##   entry_date = col_datetime(format = ""),
##   fidelity_points = col_integer(),
##   priority_encoded = col_integer(),
##   timestamp = col_date(format = ""),
##   year = col_integer(),
##   month = col_integer(),
##   day = col_integer(),
##   supported_encoded = col_integer(),
##   type_encoded = col_integer()
## )
```

```
## See spec(...) for full column specifications.
```

```
##    user  system elapsed 
##   4.278   0.067   4.366
```

# data.table

```r
library(data.table)
pigeon_racing_dt <- fread("pigeon-racing.csv")
system.time(fread("ticket_cleint.csv"))
```

```
##    user  system elapsed 
##   1.725   0.074   1.234
```


# more advanced data.table

```r
fread("pigeon-racing.csv", drop = 4:11)
```

```
##      Pos              Breeder          Pigeon
##   1:   1        Texas Outlaws 19633-AU15-FOYS
##   2:   2       Junior Juanich   0402-AU15-JRL
##   3:   3    Jerry Allensworth  0404-AU15-VITA
##   4:   4          Alias-Alias  2013-AU15-ALIA
##   5:   5         Greg Glazier   5749-AU15-SLI
##  ---                                         
## 396: 396      Hutchins/Milner  2496-AU15-VITA
## 397: 397              Twin200  7799-AU15-VITA
## 398: 398     Mayberry Classic   5508-AU15-MAC
## 399: 399 Sierra Ranch Classic  0519-AU15-SIER
## 400: 400    Hi-Cal Connection  0798-AU15-HAWA
```

```r
fread("pigeon-racing.csv", select = c(1, 2, 3))
```

```
##      Pos              Breeder          Pigeon
##   1:   1        Texas Outlaws 19633-AU15-FOYS
##   2:   2       Junior Juanich   0402-AU15-JRL
##   3:   3    Jerry Allensworth  0404-AU15-VITA
##   4:   4          Alias-Alias  2013-AU15-ALIA
##   5:   5         Greg Glazier   5749-AU15-SLI
##  ---                                         
## 396: 396      Hutchins/Milner  2496-AU15-VITA
## 397: 397              Twin200  7799-AU15-VITA
## 398: 398     Mayberry Classic   5508-AU15-MAC
## 399: 399 Sierra Ranch Classic  0519-AU15-SIER
## 400: 400    Hi-Cal Connection  0798-AU15-HAWA
```

```r
fread("pigeon-racing.csv", drop = c("Name", "Arrival"))
```

```
##      Pos              Breeder          Pigeon Color Sex Ent   Speed
##   1:   1        Texas Outlaws 19633-AU15-FOYS  BCWF   H   1 172.155
##   2:   2       Junior Juanich   0402-AU15-JRL  SIWF   H   1 163.569
##   3:   3    Jerry Allensworth  0404-AU15-VITA    BB   H   1 163.442
##   4:   4          Alias-Alias  2013-AU15-ALIA  BBSP   H   1 163.392
##   5:   5         Greg Glazier   5749-AU15-SLI    BC   H   1 163.366
##  ---                                                               
## 396: 396      Hutchins/Milner  2496-AU15-VITA    BB   H   5  90.901
## 397: 397              Twin200  7799-AU15-VITA   SIL   H   2  87.817
## 398: 398     Mayberry Classic   5508-AU15-MAC  BBSP   H   2  83.929
## 399: 399 Sierra Ranch Classic  0519-AU15-SIER    BC   H   6  78.286
## 400: 400    Hi-Cal Connection  0798-AU15-HAWA    BB   H   5  76.677
##       To Win Eligible
##   1: 0:00:00      Yes
##   2: 0:05:21      Yes
##   3: 0:05:27      Yes
##   4: 0:05:28      Yes
##   5: 0:05:30      Yes
##  ---                 
## 396: 1:31:23      Yes
## 397: 1:38:10      Yes
## 398: 1:47:28      Yes
## 399: 2:02:34      Yes
## 400: 2:07:18      Yes
```

```r
fread("pigeon-racing.csv", select = c("Breeder", "Speed"))
```

```
##                   Breeder   Speed
##   1:        Texas Outlaws 172.155
##   2:       Junior Juanich 163.569
##   3:    Jerry Allensworth 163.442
##   4:          Alias-Alias 163.392
##   5:         Greg Glazier 163.366
##  ---                             
## 396:      Hutchins/Milner  90.901
## 397:              Twin200  87.817
## 398:     Mayberry Classic  83.929
## 399: Sierra Ranch Classic  78.286
## 400:    Hi-Cal Connection  76.677
```

