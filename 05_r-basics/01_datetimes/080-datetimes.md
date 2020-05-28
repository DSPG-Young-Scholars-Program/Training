---
output: 
  html_document: 
    keep_md: yes
---
# Dates and Times

Resources

- http://lubridate.tidyverse.org/
- https://github.com/tidyverse/lubridate

Cheat Sheets

- https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf

## lubridate


```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```


```r
ymd('2018-06-06')
```

```
## [1] "2018-06-06"
```


```r
dt_str <- '2018-06-06'
class(dt_str)
```

```
## [1] "character"
```


```r
dt_dt <- ymd(dt_str)
class(dt_dt)
```

```
## [1] "Date"
```


```r
mdy('June 6, 2018')
```

```
## [1] "2018-06-06"
```


```r
dmy('06-06-2018')
```

```
## [1] "2018-06-06"
```


```r
ymd(20190606)
```

```
## [1] "2019-06-06"
```


```r
ymd_hms('2018-06-06 10:33:55', tz = 'EDT')
```

```
## [1] "2018-06-06 14:33:55 EDT"
```

## Making datetimes from data


```r
library(nycflights13)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
flights
```

```
## # A tibble: 336,776 x 19
##     year month   day dep_time sched_dep_time dep_delay arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>
##  1  2013     1     1      517            515         2      830
##  2  2013     1     1      533            529         4      850
##  3  2013     1     1      542            540         2      923
##  4  2013     1     1      544            545        -1     1004
##  5  2013     1     1      554            600        -6      812
##  6  2013     1     1      554            558        -4      740
##  7  2013     1     1      555            600        -5      913
##  8  2013     1     1      557            600        -3      709
##  9  2013     1     1      557            600        -3      838
## 10  2013     1     1      558            600        -2      753
## # … with 336,766 more rows, and 12 more variables: sched_arr_time <int>,
## #   arr_delay <dbl>, carrier <chr>, flight <int>, tailnum <chr>,
## #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
## #   minute <dbl>, time_hour <dttm>
```


```r
flight_times <- flights %>%
    select(year, month, day, hour, minute)
flight_times
```

```
## # A tibble: 336,776 x 5
##     year month   day  hour minute
##    <int> <int> <int> <dbl>  <dbl>
##  1  2013     1     1     5     15
##  2  2013     1     1     5     29
##  3  2013     1     1     5     40
##  4  2013     1     1     5     45
##  5  2013     1     1     6      0
##  6  2013     1     1     5     58
##  7  2013     1     1     6      0
##  8  2013     1     1     6      0
##  9  2013     1     1     6      0
## 10  2013     1     1     6      0
## # … with 336,766 more rows
```


```r
flight_times %>%
    mutate(dep_dt = make_datetime(year, month, day, hour, minute))
```

```
## # A tibble: 336,776 x 6
##     year month   day  hour minute dep_dt             
##    <int> <int> <int> <dbl>  <dbl> <dttm>             
##  1  2013     1     1     5     15 2013-01-01 05:15:00
##  2  2013     1     1     5     29 2013-01-01 05:29:00
##  3  2013     1     1     5     40 2013-01-01 05:40:00
##  4  2013     1     1     5     45 2013-01-01 05:45:00
##  5  2013     1     1     6      0 2013-01-01 06:00:00
##  6  2013     1     1     5     58 2013-01-01 05:58:00
##  7  2013     1     1     6      0 2013-01-01 06:00:00
##  8  2013     1     1     6      0 2013-01-01 06:00:00
##  9  2013     1     1     6      0 2013-01-01 06:00:00
## 10  2013     1     1     6      0 2013-01-01 06:00:00
## # … with 336,766 more rows
```


```r
flights %>%
    select(year, month, day, hour, minute) %>%
    mutate(dep_dt = make_datetime(year, month, day, hour, minute))
```

```
## # A tibble: 336,776 x 6
##     year month   day  hour minute dep_dt             
##    <int> <int> <int> <dbl>  <dbl> <dttm>             
##  1  2013     1     1     5     15 2013-01-01 05:15:00
##  2  2013     1     1     5     29 2013-01-01 05:29:00
##  3  2013     1     1     5     40 2013-01-01 05:40:00
##  4  2013     1     1     5     45 2013-01-01 05:45:00
##  5  2013     1     1     6      0 2013-01-01 06:00:00
##  6  2013     1     1     5     58 2013-01-01 05:58:00
##  7  2013     1     1     6      0 2013-01-01 06:00:00
##  8  2013     1     1     6      0 2013-01-01 06:00:00
##  9  2013     1     1     6      0 2013-01-01 06:00:00
## 10  2013     1     1     6      0 2013-01-01 06:00:00
## # … with 336,766 more rows
```

## Non-standard date formatting


```r
# an example of a non-standard date format
'WED 06-JUNE-18 10:47:30 AM'
```

```
## [1] "WED 06-JUNE-18 10:47:30 AM"
```

If you look at the `as_datetime` function, under the 'format' section there is a link to the `strptime` documentation


```r
?lubridate::as_datetime
```

If you look at the `strptime` documentation, you will get a nice table of codes you can use to create your own datetime pattern


```r
?strptime
```

You can now use these variables to create a pattern for your custom datetime string


```r
curr_time <- lubridate::as_datetime('WED 06-JUNE-18 10:47:30 AM',
                       format = '%a %d-%B-%y %I:%M:%S %p',
                       tz = "EST")
curr_time
```

```
## [1] "2018-06-06 10:47:30 EST"
```


## strptime format variables

The table of values in strptime have been reproduced below

```
%a
Abbreviated weekday name in the current locale on this platform. (Also matches full name on input: in some locales there are no abbreviations of names.)

%A
Full weekday name in the current locale. (Also matches abbreviated name on input.)

%b
Abbreviated month name in the current locale on this platform. (Also matches full name on input: in some locales there are no abbreviations of names.)

%B
Full month name in the current locale. (Also matches abbreviated name on input.)

%c
Date and time. Locale-specific on output, "%a %b %e %H:%M:%S %Y" on input.

%C
Century (00–99): the integer part of the year divided by 100.

%d
Day of the month as decimal number (01–31).

%D
Date format such as %m/%d/%y: the C99 standard says it should be that exact format (but not all OSes comply).

%e
Day of the month as decimal number (1–31), with a leading space for a single-digit number.

%F
Equivalent to %Y-%m-%d (the ISO 8601 date format).

%g
The last two digits of the week-based year (see %V). (Accepted but ignored on input.)

%G
The week-based year (see %V) as a decimal number. (Accepted but ignored on input.)

%h
Equivalent to %b.

%H
Hours as decimal number (00–23). As a special exception strings such as 24:00:00 are accepted for input, since ISO 8601 allows these.

%I
Hours as decimal number (01–12).

%j
Day of year as decimal number (001–366).

%m
Month as decimal number (01–12).

%M
Minute as decimal number (00–59).

%n
Newline on output, arbitrary whitespace on input.

%p
AM/PM indicator in the locale. Used in conjunction with %I and not with %H. An empty string in some locales (for example on some OSes, non-English European locales including Russia). The behaviour is undefined if used for input in such a locale.

Some platforms accept %P for output, which uses a lower-case version (%p may also use lower case): others will output P.

%r
For output, the 12-hour clock time (using the locale's AM or PM): only defined in some locales, and on some OSes misleading in locales which do not define an AM/PM indicator. For input, equivalent to %I:%M:%S %p.

%R
Equivalent to %H:%M.

%S
Second as integer (00–61), allowing for up to two leap-seconds (but POSIX-compliant implementations will ignore leap seconds).

%t
Tab on output, arbitrary whitespace on input.

%T
Equivalent to %H:%M:%S.

%u
Weekday as a decimal number (1–7, Monday is 1).

%U
Week of the year as decimal number (00–53) using Sunday as the first day 1 of the week (and typically with the first Sunday of the year as day 1 of week 1). The US convention.

%V
Week of the year as decimal number (01–53) as defined in ISO 8601. If the week (starting on Monday) containing 1 January has four or more days in the new year, then it is considered week 1. Otherwise, it is the last week of the previous year, and the next week is week 1. (Accepted but ignored on input.)

%w
Weekday as decimal number (0–6, Sunday is 0).

%W
Week of the year as decimal number (00–53) using Monday as the first day of week (and typically with the first Monday of the year as day 1 of week 1). The UK convention.

%x
Date. Locale-specific on output, "%y/%m/%d" on input.

%X
Time. Locale-specific on output, "%H:%M:%S" on input.

%y
Year without century (00–99). On input, values 00 to 68 are prefixed by 20 and 69 to 99 by 19 – that is the behaviour specified by the 2004 and 2008 POSIX standards, but they do also say ‘it is expected that in a future version the default century inferred from a 2-digit year will change’.

%Y
Year with century. Note that whereas there was no zero in the original Gregorian calendar, ISO 8601:2004 defines it to be valid (interpreted as 1BC): see https://en.wikipedia.org/wiki/0_(year). Note that the standards also say that years before 1582 in its calendar should only be used with agreement of the parties involved.

For input, only years 0:9999 are accepted.

%z
Signed offset in hours and minutes from UTC, so -0800 is 8 hours behind UTC. Values up to +1400 are accepted. (Standard only for output.)

%Z
(Output only.) Time zone abbreviation as a character string (empty if not available). This may not be reliable when a time zone has changed abbreviations over the years.


```

## Datetime arithmetic

Once you have a datetime object, you can then begin to do calculations and arithmetic on them.


```r
now() - curr_time
```

```
## Time difference of 477.0105 days
```
