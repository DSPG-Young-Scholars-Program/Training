---
title: "Rename with base, dplyr & data.table"
output: 
  html_document: 
    keep_md: yes
---



# renaming column base

```r
df <- data.frame(column_1 = c(1, 2, 3), column_b = c("A", "B", "C"))
print(df)
```

```
##   column_1 column_b
## 1        1        A
## 2        2        B
## 3        3        C
```

```r
names(df)[names(df) == 'column_b'] <- 'column_2'
print(df)
```

```
##   column_1 column_2
## 1        1        A
## 2        2        B
## 3        3        C
```

# renaming column dplyr

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
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
df <- data.frame(column_1 = c(1, 2, 3), column_b = c("A", "B", "C"))
print(df)
```

```
##   column_1 column_b
## 1        1        A
## 2        2        B
## 3        3        C
```

```r
df <- dplyr::rename(df, "column_2" = column_b)
print(df)
```

```
##   column_1 column_2
## 1        1        A
## 2        2        B
## 3        3        C
```

# renaming column data.table

```r
library(data.table)
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
df <- data.frame(column_1 = c(1, 2, 3), column_b = c("A", "B", "C"))
print(df)
```

```
##   column_1 column_b
## 1        1        A
## 2        2        B
## 3        3        C
```

```r
setnames(df, "column_b", "column_2")
print(df)
```

```
##   column_1 column_2
## 1        1        A
## 2        2        B
## 3        3        C
```
