---
title: "Merge data sets with base, dplyr (tidyverse) & data.table"
output:
  html_document: 
    keep_md: yes
---


```r
library(data.table) # 1.11.5, 2018-06-02 00:09:06 UTC
library(dplyr) # 0.7.5.9000, 2018-06-12 01:41:40 UTC
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
set.seed(1L)
names = paste0("V", 1:200)
cols = 200L
foo <- function() {
    data = as.data.frame(setDT(lapply(1:cols, function(x) sample(10))))
    setnames(data, sample(names))
}
n = 10e3L
ll = vector("list", n)
for (i in 1:n) {
    .Call("Csetlistelt", ll, i, foo())
}
```


```r
system.time(ans3 <- do.call("rbind", ll))
```

```
##    user  system elapsed 
##  41.332   0.286  41.877
```


```r
system.time(ans4 <- dplyr::bind_rows(ll))
```

```
##    user  system elapsed 
##  14.438   0.007  14.458
```


```r
system.time(ans1 <- rbindlist(ll))
```

```
##    user  system elapsed 
##   3.052   0.007   3.060
```


```r
system.time(ans2 <- rbindlist(ll, use.names=TRUE))
```

```
##    user  system elapsed 
##   3.183   0.082   3.267
```

