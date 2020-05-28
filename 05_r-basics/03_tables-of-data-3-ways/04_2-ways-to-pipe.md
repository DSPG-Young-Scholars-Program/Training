---
title: "Piping (chaining) with magrittr & data.table"
output:
  html_document: 
    keep_md: yes
---

## Nested Option:

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
arrange(
  summarize(
    group_by(
      filter(mtcars, carb > 1),cyl), Avg_mpg = mean(mpg)), desc(Avg_mpg)
  )
```

```
## # A tibble: 3 x 2
##     cyl Avg_mpg
##   <dbl>   <dbl>
## 1     4    25.9
## 2     6    19.7
## 3     8    15.1
```



## Multiple Object Option

```r
a <- filter(mtcars, carb > 1) 
b <- group_by(a, cyl) 
c <- summarise(b, Avg_mpg = mean(mpg)) 
d <- arrange(c, desc(Avg_mpg)) 
print(d)
```

```
## # A tibble: 3 x 2
##     cyl Avg_mpg
##   <dbl>   <dbl>
## 1     4    25.9
## 2     6    19.7
## 3     8    15.1
```



## %>% Option:

```r
library(magrittr) 
library(dplyr) 
mtcars %>% 
  filter(carb > 1) %>% 
  group_by(cyl) %>% 
  summarise(Avg_mpg = mean(mpg)) %>% 
  arrange(desc(Avg_mpg))
```

```
## # A tibble: 3 x 2
##     cyl Avg_mpg
##   <dbl>   <dbl>
## 1     4    25.9
## 2     6    19.7
## 3     8    15.1
```


## . is assumed first

```r
mtcars %>% 
  filter(carb > 1) %>% 
  lm(mpg ~ cyl + hp, data = .) %>% 
  summary()
```

```
## 
## Call:
## lm(formula = mpg ~ cyl + hp, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.6163 -1.4162 -0.1506  1.6181  5.2021 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 35.67647    2.28382  15.621 2.16e-13 ***
## cyl         -2.22014    0.52619  -4.219 0.000353 ***
## hp          -0.01414    0.01323  -1.069 0.296633    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.689 on 22 degrees of freedom
## Multiple R-squared:  0.7601,	Adjusted R-squared:  0.7383 
## F-statistic: 34.85 on 2 and 22 DF,  p-value: 1.516e-07
```



##  data.table piping

```r
set.seed(123)
dt <- data.table(a = sample(letters, 1e5, replace = TRUE),
                 b = abs(rnorm(1e5)))

dt
```

```
##         a         b
##      1: h 0.2595897
##      2: u 0.9175107
##      3: k 0.7223183
##      4: w 0.8082840
##      5: y 0.1413520
##     ---            
##  99996: y 0.1818110
##  99997: m 0.6413557
##  99998: f 0.3018467
##  99999: y 1.2823859
## 100000: c 2.1735285
```

##  one line

```r
# adding columns in dt uses :=, $ will still work as well but not for piping
dt[, x := sqrt(b)][, y := b^2][, z := paste0(a , b)]

dt
```

```
##         a         b         x          y                  z
##      1: h 0.2595897 0.5094995 0.06738683 h0.259589727801875
##      2: u 0.9175107 0.9578678 0.84182591 u0.917510715589297
##      3: k 0.7223183 0.8498931 0.52174379 k0.722318344933023
##      4: w 0.8082840 0.8990462 0.65332306 w0.808284024409799
##      5: y 0.1413520 0.3759681 0.01998039 y0.141352016059972
##     ---                                                    
##  99996: y 0.1818110 0.4263931 0.03305525 y0.181811036746857
##  99997: m 0.6413557 0.8008468 0.41133708 m0.641355659540064
##  99998: f 0.3018467 0.5494058 0.09111145 f0.301846730984395
##  99999: y 1.2823859 1.1324248 1.64451364  y1.28238591533624
## 100000: c 2.1735285 1.4742891 4.72422609  c2.17352848752259
```

# multiple lines - ewwwww

```r
# multiline dt piping is hard to read
dt[, x := sqrt(b)][
  , y := b^2][
    , z := paste0(a , b)]

dt
```

```
##         a         b         x          y                  z
##      1: h 0.2595897 0.5094995 0.06738683 h0.259589727801875
##      2: u 0.9175107 0.9578678 0.84182591 u0.917510715589297
##      3: k 0.7223183 0.8498931 0.52174379 k0.722318344933023
##      4: w 0.8082840 0.8990462 0.65332306 w0.808284024409799
##      5: y 0.1413520 0.3759681 0.01998039 y0.141352016059972
##     ---                                                    
##  99996: y 0.1818110 0.4263931 0.03305525 y0.181811036746857
##  99997: m 0.6413557 0.8008468 0.41133708 m0.641355659540064
##  99998: f 0.3018467 0.5494058 0.09111145 f0.301846730984395
##  99999: y 1.2823859 1.1324248 1.64451364  y1.28238591533624
## 100000: c 2.1735285 1.4742891 4.72422609  c2.17352848752259
```

# using magrittr pipes with data.table :-)

```r
# Best of both worlds!
dt[, x := sqrt(b)] %>%
  .[, y := b^2] %>%
  .[, z := paste0(a , b)]

dt
```

```
##         a         b         x          y                  z
##      1: h 0.2595897 0.5094995 0.06738683 h0.259589727801875
##      2: u 0.9175107 0.9578678 0.84182591 u0.917510715589297
##      3: k 0.7223183 0.8498931 0.52174379 k0.722318344933023
##      4: w 0.8082840 0.8990462 0.65332306 w0.808284024409799
##      5: y 0.1413520 0.3759681 0.01998039 y0.141352016059972
##     ---                                                    
##  99996: y 0.1818110 0.4263931 0.03305525 y0.181811036746857
##  99997: m 0.6413557 0.8008468 0.41133708 m0.641355659540064
##  99998: f 0.3018467 0.5494058 0.09111145 f0.301846730984395
##  99999: y 1.2823859 1.1324248 1.64451364  y1.28238591533624
## 100000: c 2.1735285 1.4742891 4.72422609  c2.17352848752259
```

