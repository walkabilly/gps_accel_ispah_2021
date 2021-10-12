---
title: "Accelerometer and GPS data wrangling"
author: "Daniel Fuller"
date: "08/10/2021"
output:
      html_document:
        keep_md: true
---




```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
## ✓ tibble  3.1.2     ✓ dplyr   1.0.6
## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(ggmap)
```

```
## Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
```

```
## Please cite ggmap if you use it! See citation("ggmap") for details.
```

```r
library(stringr)
library(data.table)
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
##     yday, year
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```
## The following object is masked from 'package:purrr':
## 
##     transpose
```

```r
library(scales)
```

```
## 
## Attaching package: 'scales'
```

```
## The following object is masked from 'package:purrr':
## 
##     discard
```

```
## The following object is masked from 'package:readr':
## 
##     col_factor
```

```r
library(signal)
```

```
## 
## Attaching package: 'signal'
```

```
## The following object is masked from 'package:dplyr':
## 
##     filter
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, poly
```

```r
library(magrittr)
```

```
## 
## Attaching package: 'magrittr'
```

```
## The following object is masked from 'package:ggmap':
## 
##     inset
```

```
## The following object is masked from 'package:purrr':
## 
##     set_names
```

```
## The following object is masked from 'package:tidyr':
## 
##     extract
```

```r
library(furrr)
```

```
## Loading required package: future
```

```r
library(purrr)
library(activityCounts)
```

# Accelerometer Data Wrangling

These data are from Ethica Data, which is app manufacturer our INTERACT team has been working with for the past 7 years. 


```r
accel_data <- read.csv("accel_sample_data.csv")

glimpse(accel_data)
```

```
## Rows: 1,643,202
## Columns: 10
## $ X           <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
## $ user_id     <int> 2836, 2836, 2836, 2836, 2836, 2836, 2836, 2836, 2836, 2836…
## $ date        <chr> "2017-11", "2017-11", "2017-11", "2017-11", "2017-11", "20…
## $ device_id   <chr> "DF184FF5942648B3AEA0AB98684E9F86", "DF184FF5942648B3AEA0A…
## $ record_time <chr> "2017-11-09 14:23:48", "2017-11-09 14:23:48", "2017-11-09 …
## $ timestamp   <chr> "47a61080-c55f-11e7-8d83-191d1de0632f", "47bb1f20-c55f-11e…
## $ accu        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ x_axis      <dbl> 0.03725976, 0.04129998, 0.04833294, 0.06045359, 0.05746084…
## $ y_axis      <dbl> 0.02304419, 0.01556231, 0.03561375, 0.04908114, 0.02902970…
## $ z_axis      <dbl> 9.843910, 9.821165, 9.846004, 9.842862, 9.825953, 9.825205…
```

```r
table(accel_data$user_id)
```

```
## 
##   2836   3244 
## 860688 782514
```

# Accel data processing

Tasks

1. Decide which method we are going to use for data processing
    * ActiGraph Activity Counts (AAC) - [https://cran.r-project.org/web/packages/activityCounts/index.html](https://cran.r-project.org/web/packages/activityCounts/index.html)
    * Euclidean Norm Minus One (ENMO) - [https://cran.r-project.org/web/packages/GGIR/index.html](https://cran.r-project.org/web/packages/GGIR/index.html)
    * Monitor Independent Movement Summary (MIMS) [https://cran.r-project.org/web/packages/MIMSunit/index.html](https://cran.r-project.org/web/packages/MIMSunit/index.html)
    
### Sorting out time

These data are different from most accelerometer data because the phone does not capture accelerometer data like a traditional research grade device. It optimizes for battery life and often does not have a consistent number of hertz per second. So unlike data were you will set the hertz to 30 or 100 this will be different. 

# ou will probably have to resample the data... but we are not going to do that today because don't have time. 


```r
accel_data$time_factor <- as.factor(accel_data$record_time)

accel_data <- accel_data %>%
  group_by(record_time) %>%
  mutate(
         milli = row_number(),
         n = n()
  )

summary(accel_data$milli)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   5.000   4.999   7.000  97.000
```

# Dealing with time


```r
accel_data$day <- day(accel_data$record_time)  ## Create an hour variable
accel_data$hour <- hour(accel_data$record_time)  ## Create an hour variable
accel_data$minute <- minute(accel_data$record_time)  ## Create a minute variable
accel_data$second <- second(accel_data$record_time)  ## Create a second variable
```

# Calculating ENMO


```r
accel_data <- accel_data %>%
                mutate(
                  x_g = x_axis/9.80665,
                  y_g = y_axis/9.80665,
                  z_g = z_axis/9.80665,
                  enmo = x_g^2+y_g^2+z_g^2,
                  enmo_m1 = (x_g^2+y_g^2+z_g^2)-1
                )
```


```r
summary(accel_data$enmo_m1)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.99653  0.00034  0.00238  0.00391  0.00438 70.11597
```

```r
accel_data <- dplyr::filter(accel_data, enmo_m1 < 20)

summary(accel_data$enmo_m1)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## -0.996526  0.000339  0.002383  0.003753  0.004383 19.905638
```


```r
accel_sec <- accel_data %>%
  group_by(user_id, day, hour, minute, second) %>%
  summarise(
         time = first(record_time), 
         user_id = first(user_id),
         m_x_g = mean(x_g),
         m_y_g = mean(y_g),
         m_z_g = mean(z_g),
         enmo_m1 = mean(abs(enmo_m1)),
         enmo_m1_mg = enmo_m1*1000
        )
```

```
## `summarise()` has grouped output by 'user_id', 'day', 'hour', 'minute'. You can override using the `.groups` argument.
```

```r
summary(accel_sec$enmo_m1_mg)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##    0.007    1.808    2.831    7.989    4.291 4250.402
```

```r
ggplot(accel_sec, aes(enmo_m1_mg)) +
        geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](wrangling_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#### 5. Use the cut points from this [paper](https://doi.org/10.3389/fspor.2020.579278) and create a new variable called `activity`. 

| Activity | Cut Point (enmo_m1) | 
|----------|-----------|
| Sedentary | <= 255 |
| Moderate | > 255 to <= 588 | 
| Moderate | >= 588 to max |

*Note. Don't do this at home. Should make sure your sample frequency is correct for the method you are using.*


```r
accel_sec <- accel_sec %>%
	mutate(activity = case_when(
		enmo_m1_mg <= 255 ~ "1. Sedentary",
		enmo_m1_mg > 255 & enmo_m1_mg <= 588 ~ "2. Light",
		enmo_m1_mg > 588 ~ "3. Moderate",
		TRUE ~ "other"
	))

table(accel_sec$activity)
```

```
## 
## 1. Sedentary     2. Light  3. Moderate 
##       198322          864          389
```

```{}
fig1 <- ggplot(accel_sec, aes(x = time, y = enmo_m1)) + 
          geom_point(alpha = 0.01, aes(colour = factor(activity))) + 
            xlab("Time") +
            ylab("ENMO") +
        facet_wrap(~ user_id)
plot(fig1)
```

# GPS Sample Data


```r
gps_data <- read_csv("gps_sample_data.csv")
```

```
## Warning: Missing column names filled in: 'X1' [1]
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   X1 = col_double(),
##   user_id = col_double(),
##   date = col_character(),
##   device_id = col_character(),
##   record_time = col_datetime(format = ""),
##   timestamp = col_character(),
##   accu = col_double(),
##   alt = col_double(),
##   bearing = col_double(),
##   lat = col_double(),
##   lon = col_double(),
##   provider = col_character(),
##   satellite_time = col_datetime(format = ""),
##   speed = col_double()
## )
```

```r
glimpse(gps_data)
```

```
## Rows: 6,204
## Columns: 14
## $ X1             <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
## $ user_id        <dbl> 2836, 2836, 2836, 2836, 2836, 2836, 2836, 2836, 2836, 2…
## $ date           <chr> "2017-11", "2017-11", "2017-11", "2017-11", "2017-11", …
## $ device_id      <chr> "DF184FF5942648B3AEA0AB98684E9F86", "DF184FF5942648B3AE…
## $ record_time    <dttm> 2017-11-09 14:23:48, 2017-11-09 14:23:48, 2017-11-09 1…
## $ timestamp      <chr> "47a9ba00-c55f-11e7-8d83-191d1de0632f", "47a9ba02-c55f-…
## $ accu           <dbl> 65.0000, 65.0000, 65.0000, 65.0000, 65.0000, 65.0000, 6…
## $ alt            <dbl> 66.44848, 66.44848, 66.49751, 66.52485, 66.47801, 66.67…
## $ bearing        <dbl> -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,…
## $ lat            <dbl> 47.57115, 47.57115, 47.57115, 47.57115, 47.57115, 47.57…
## $ lon            <dbl> -52.73409, -52.73409, -52.73407, -52.73407, -52.73408, …
## $ provider       <chr> "gps", "gps", "gps", "gps", "gps", "gps", "gps", "gps",…
## $ satellite_time <dttm> 2017-11-09 14:17:25, 2017-11-09 14:23:48, 2017-11-09 1…
## $ speed          <dbl> -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,…
```


```r
gps_plot_1 <- ggplot(gps_data) + 
                  geom_point(aes(x = lon, y = lat)) +
                  facet_wrap(~ user_id)
plot(gps_plot_1)
```

![](wrangling_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
canada_basemap <- get_map(location = "St. John's, Newfoundland, Canada",
                     source = "google",
                     maptype = "roadmap", crop = FALSE,
                     zoom = 12)
```

```
## Source : https://maps.googleapis.com/maps/api/staticmap?center=St.%20John's,%20Newfoundland,%20Canada&zoom=12&size=640x640&scale=2&maptype=roadmap&language=en-EN&key=xxx-9rGQuKs
```

```
## Source : https://maps.googleapis.com/maps/api/geocode/json?address=St.+John's,+Newfoundland,+Canada&key=xxx-9rGQuKs
```

```r
maps_points <- ggmap(canada_basemap) + 
                  geom_point(aes(x = lon, y = lat), data = gps_data, alpha = 0.2) + 
                  facet_wrap(~ user_id)

plot(maps_points)
```

![](wrangling_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

