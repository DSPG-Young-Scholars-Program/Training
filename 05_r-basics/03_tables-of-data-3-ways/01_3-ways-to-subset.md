---
title: "Subset data sets with base, dplyr (tidyverse) & data.table"
output:
  html_document: 
    keep_md: yes
---



## Load Libraries

```r
library(dplyr)
library(data.table)
```

## A data frame, the basic tabular representation of data in R

```r
n = c(2, 3, 5) 
s = c("aa", "bb", "cc") 
b = c(TRUE, FALSE, TRUE) 
df = data.frame(n, s, b)       # df is a data frame
df
```

```
##   n  s     b
## 1 2 aa  TRUE
## 2 3 bb FALSE
## 3 5 cc  TRUE
```


## Create Sample Data Frame

```r
df <- data.frame(column_1 = c(1, 2, 3),
                 column_2 = c("A", "B", "C"),
                 column_3 = c("D", "E", "F"),
                 stringsAsFactors = F)
df
```

```
##   column_1 column_2 column_3
## 1        1        A        D
## 2        2        B        E
## 3        3        C        F
```

```r
#pg <- fread("pigeon-racing.csv")
```

# Subset Rows
## base R

```r
df[df$column_1 == 2,] # have to include df$ before column name and don't forget the comma!
```

```
##   column_1 column_2 column_3
## 2        2        B        E
```

```r
df[df$column_1 == 2,]
```

```
##   column_1 column_2 column_3
## 2        2        B        E
```

## dplyr

```r
library(dplyr)
filter(df, column_1 == 2)
```

```
##   column_1 column_2 column_3
## 1        2        B        E
```

```r
filter(df, column_1 == 2)
```

```
##   column_1 column_2 column_3
## 1        2        B        E
```

## data.table

```r
library(data.table)
setDT(df) # needs to be a data.table first
df[column_1 == 2]
```

```
##    column_1 column_2 column_3
## 1:        2        B        E
```

```r
df[column_1 == 2]
```

```
##    column_1 column_2 column_3
## 1:        2        B        E
```

# Subset Columns
## base R

```r
df[, c("column_2", "column_3")]
```

```
##    column_2 column_3
## 1:        A        D
## 2:        B        E
## 3:        C        F
```

## dplyr

```r
select(df, c("column_2", "column_3"))
```

```
##    column_2 column_3
## 1:        A        D
## 2:        B        E
## 3:        C        F
```

## data.table

```r
setDT(df) # needs to be a data.table first
df[, .(column_2, column_3)] # the dot . is a shortcut for list()
```

```
##    column_2 column_3
## 1:        A        D
## 2:        B        E
## 3:        C        F
```

```r
df[, .(column_2, column_3)]
```

```
##    column_2 column_3
## 1:        A        D
## 2:        B        E
## 3:        C        F
```

```r
# or just like a data.frame
df[, c("column_2", "column_3")]
```

```
##    column_2 column_3
## 1:        A        D
## 2:        B        E
## 3:        C        F
```

# Subset Rows & Columns
## base R

```r
df[df$column_1 == 2, c("column_2", "column_3")]
```

```
##    column_2 column_3
## 1:        B        E
```

## dplyr

```r
library(dplyr)

# two steps using the magrittr "pipe"
df %>%
  filter(column_1 == 2) %>%
  select(c("column_2", "column_3")) # don't need to specifcy data.frame in second step
```

```
##   column_2 column_3
## 1        B        E
```

## data.table

```r
setDT(df) # needs to be a data.table first

df[column_1 == 2, .(column_2, column_3)] # the dot . is a shortcut for list()
```

```
##    column_2 column_3
## 1:        B        E
```

```r
# or just like a data.frame
df[df$column_1 == 2, c("column_2", "column_3")]
```

```
##    column_2 column_3
## 1:        B        E
```


