HW5 Part 1: Practice functions and debugging code
================
Sophie Ennis

## Overview

This script includes code to analyze the popularity of baby name in the
United States using the `babynames` package (data info:
<http://hadley.github.io/babynames/>).

This package has three datasets provided by the US government. Only the
first two will be will be used for this assignment: \* `babynames` \*
`applicants` \* `lifetables`

This script has some bugs and currently does not work. Your task is to
fix these mistakes to generate the desired output.

## Load necessary libraries

If you are working on your local R Studio, you might need to install the
libraries before loading them. If you are working on Workbench, simply
run the code below to load the libraries, no installation required.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0     ✔ purrr   1.0.2
    ## ✔ tibble  3.2.1     ✔ dplyr   1.1.4
    ## ✔ tidyr   1.3.0     ✔ stringr 1.5.1
    ## ✔ readr   2.1.5     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(babynames)
```

Check data (modify this code as you prefer)

``` r
glimpse(babynames)
```

    ## Rows: 1,924,665
    ## Columns: 5
    ## $ year <dbl> 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880,…
    ## $ sex  <chr> "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", …
    ## $ name <chr> "Mary", "Anna", "Emma", "Elizabeth", "Minnie", "Margaret", "Ida",…
    ## $ n    <int> 7065, 2604, 2003, 1939, 1746, 1578, 1472, 1414, 1320, 1288, 1258,…
    ## $ prop <dbl> 0.07238359, 0.02667896, 0.02052149, 0.01986579, 0.01788843, 0.016…

``` r
glimpse(applicants)
```

    ## Rows: 276
    ## Columns: 3
    ## $ year  <int> 1880, 1880, 1881, 1881, 1882, 1882, 1883, 1883, 1884, 1884, 1885…
    ## $ sex   <chr> "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F",…
    ## $ n_all <int> 97605, 118400, 98855, 108282, 115695, 122031, 120059, 112477, 13…

``` r
str(babynames)
```

    ## tibble [1,924,665 × 5] (S3: tbl_df/tbl/data.frame)
    ##  $ year: num [1:1924665] 1880 1880 1880 1880 1880 1880 1880 1880 1880 1880 ...
    ##  $ sex : chr [1:1924665] "F" "F" "F" "F" ...
    ##  $ name: chr [1:1924665] "Mary" "Anna" "Emma" "Elizabeth" ...
    ##  $ n   : int [1:1924665] 7065 2604 2003 1939 1746 1578 1472 1414 1320 1288 ...
    ##  $ prop: num [1:1924665] 0.0724 0.0267 0.0205 0.0199 0.0179 ...

``` r
str(applicants)
```

    ## tibble [276 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ year : int [1:276] 1880 1880 1881 1881 1882 1882 1883 1883 1884 1884 ...
    ##  $ sex  : chr [1:276] "F" "M" "F" "M" ...
    ##  $ n_all: int [1:276] 97605 118400 98855 108282 115695 122031 120059 112477 137586 122738 ...

### QUESTION 1. The code below uses the `applicants` data to generate a line plot that displays the total US births by sex and year. The code contains TWO errors. Find and fix them. In a few sentences, explain why each of the errors occurred.

``` r
#fixed
applicants %>%
  mutate( 
    sex = if_else(sex == "F", "Female", "Male"), 
  ) %>%
  ggplot(mapping = aes(x = year, y = n_all, color = sex)) + 
  geom_line() +
  labs(
    title = "Total US births by sex and year",
    x = "Year",
    y = "Number of births",
    caption = "Source: Social Security Administration"
  ) +
  theme_minimal()
```

![](fix-errors_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

While using if_else(), we don’t want to set “F” equal to sex, we want to
assign sex to be “Female” when sex is “F”. This is why we use two `==`
and not one `=`. When we’re assigning a variable to the color in ggplot
graphs, we want ggplot to read the variable as a column name. When we
use quotations around the variable, ggplot reads the variable as a
literal color and instead of differentiating observations within the
column with different colors, it will set them all as the same color.
That is why we don’t use quotations around the variable when we want to
differentiate between observations.

### QUESTION 2. The code below calculates the mean for all variables in the `applicants` dataframe using a `for loop`. The code contains TWO errors and shows ONE warning; refer to slides and readings for the difference between errors and warnings. Find and fix them (different approaches are possible here, especially for the warning). In a few sentences, describe why each problem occurred.

``` r
#fixed
output <- vector(mode = "double" , length = length(applicants))
output
```

    ## [1] 0 0 0

``` r
for (i in seq_along(applicants)) { 
  if (is.numeric(applicants[[i]])) {
    output[i] <- mean(applicants[[i]], na.rm = TRUE)
  }
  else {
    output[i] <- "variable is not numeric"
  }}
output
```

    ## [1] "1948.5"                  "variable is not numeric"
    ## [3] "1323697.30072464"

The two errors were that the for loop was looping over the elements
rather than the indices so the for loop was trying to iterate over the
variables rather than the values and throwing an error. I added
`seq_along()` and double brackets to remedy this. The warning was caused
by one of the columns not having numeric values, returning that one of
the arguments is not numeric or logical and returning NA. To remedy this
I added conditional statements having the for loop evaluate the mean
only when the argument was numeric and otherwise returning that the
variable is not numeric.

### QUESTION 3. The code below defines a function to create a bar plot with the total count of observations for a given variable in the dataset `babynames`. Specifically, it shows the total number of births by sex and the total number of births by year. The code contains THREE errors. Find and fix them. In a few sentences, describe why each problem occurred.

``` r
#fixed
summarize_and_plot <- function(df, column) {
  summary_data <- df %>%
    group_by({{ column }}) %>% 
    summarise(total_births = sum(n)) 
  ggplot(summary_data, aes(x = {{ column }}, y = total_births)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()
}
# call the function first by sex, then by year
summarize_and_plot(babynames, sex)
```

![](fix-errors_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
summarize_and_plot(babynames, year)
```

![](fix-errors_files/figure-gfm/unnamed-chunk-5-2.png)<!-- --> The
`column` inside the `group_by()` wasn’t inside the curly brackets so
instead of the function looking at the variables inside the columns it
was looking at a variable called column. In the `summarize` line the sum
needs to be taken of n instead of just `sum()` because there was no data
being acted on and it was taking a sum of nothing. `geom_bar()` needs to
have `stat = "identity"` because it matches the height of the bars to
the values in the data, which means that the bars in the bar graph
weren’t matched to the data.

### QUESTION 4. After correcting the errors in the previous question, rewrite the code to produce the exact same output for both variables (sex and year), but **do not use a function** this time. Then, briefly answer the following questions: What differences do you notice between using a function and not using a function for this task? Which approach do you prefer and why? There are no right or wrong answers to these two questions; we are interested in what you learned and your thoughts on each approach.

``` r
babynames %>% 
  group_by(year) %>% 
  summarize(total_births = sum(n)) %>% 
  ggplot(aes(
    x = year, y = total_births)) +
  geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()
```

![](fix-errors_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
babynames %>% 
  group_by(sex) %>% 
  summarize(total_births = sum(n)) %>% 
  ggplot(aes(
    x = sex, y = total_births)) +
  geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()
```

![](fix-errors_files/figure-gfm/unnamed-chunk-6-2.png)<!-- --> While
writing a function is more complicated, it is able to perform operations
on two variables at once. For the function, I had to use curly brackets
to look deep within the columns whereas with just ggplot I called on
each column separately. I definitely prefer writing code without
functions because it is far more intuitive for me having learned
ggplot() first but I can see the advantages to using functions because
of its efficiency.

## Session info

``` r
sessioninfo::session_info()
```

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value
    ##  version  R version 4.2.0 (2022-04-22)
    ##  os       Red Hat Enterprise Linux 8.10 (Ootpa)
    ##  system   x86_64, linux-gnu
    ##  ui       X11
    ##  language (EN)
    ##  collate  en_US.UTF-8
    ##  ctype    en_US.UTF-8
    ##  tz       America/Chicago
    ##  date     2024-11-14
    ##  pandoc   2.17.1.1 @ /usr/lib/rstudio-server/bin/quarto/bin/ (via rmarkdown)
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package       * version date (UTC) lib source
    ##  assertthat      0.2.1   2019-03-21 [2] CRAN (R 4.2.0)
    ##  babynames     * 1.0.1   2021-04-12 [2] CRAN (R 4.2.0)
    ##  backports       1.4.1   2021-12-13 [2] CRAN (R 4.2.0)
    ##  broom           1.0.1   2022-08-29 [2] CRAN (R 4.2.0)
    ##  cellranger      1.1.0   2016-07-27 [2] CRAN (R 4.2.0)
    ##  cli             3.6.3   2024-06-21 [2] CRAN (R 4.2.0)
    ##  colorspace      2.0-3   2022-02-21 [2] CRAN (R 4.2.0)
    ##  crayon          1.5.2   2022-09-29 [2] CRAN (R 4.2.0)
    ##  DBI             1.1.3   2022-06-18 [2] CRAN (R 4.2.0)
    ##  dbplyr          2.2.1   2022-06-27 [2] CRAN (R 4.2.0)
    ##  digest          0.6.29  2021-12-01 [2] CRAN (R 4.2.0)
    ##  dplyr         * 1.1.4   2023-11-17 [2] CRAN (R 4.2.0)
    ##  evaluate        0.16    2022-08-09 [2] CRAN (R 4.2.0)
    ##  fansi           1.0.6   2023-12-08 [2] CRAN (R 4.2.0)
    ##  farver          2.1.1   2022-07-06 [2] CRAN (R 4.2.0)
    ##  fastmap         1.2.0   2024-05-15 [2] CRAN (R 4.2.0)
    ##  forcats       * 0.5.2   2022-08-19 [2] CRAN (R 4.2.0)
    ##  fs              1.5.2   2021-12-08 [2] CRAN (R 4.2.0)
    ##  gargle          1.2.1   2022-09-08 [2] CRAN (R 4.2.0)
    ##  generics        0.1.3   2022-07-05 [2] CRAN (R 4.2.0)
    ##  ggplot2       * 3.4.0   2022-11-04 [2] CRAN (R 4.2.0)
    ##  glue            1.7.0   2024-01-09 [2] CRAN (R 4.2.0)
    ##  googledrive     2.0.0   2021-07-08 [2] CRAN (R 4.2.0)
    ##  googlesheets4   1.0.1   2022-08-13 [2] CRAN (R 4.2.0)
    ##  gtable          0.3.1   2022-09-01 [2] CRAN (R 4.2.0)
    ##  haven           2.5.1   2022-08-22 [2] CRAN (R 4.2.0)
    ##  highr           0.11    2024-05-26 [2] CRAN (R 4.2.0)
    ##  hms             1.1.3   2023-03-21 [2] CRAN (R 4.2.0)
    ##  htmltools       0.5.8.1 2024-04-04 [2] CRAN (R 4.2.0)
    ##  httr            1.4.4   2022-08-17 [2] CRAN (R 4.2.0)
    ##  jsonlite        1.8.0   2022-02-22 [2] CRAN (R 4.2.0)
    ##  knitr           1.48    2024-07-07 [2] CRAN (R 4.2.0)
    ##  labeling        0.4.2   2020-10-20 [2] CRAN (R 4.2.0)
    ##  lifecycle       1.0.4   2023-11-07 [2] CRAN (R 4.2.0)
    ##  lubridate       1.8.0   2021-10-07 [2] CRAN (R 4.2.0)
    ##  magrittr        2.0.3   2022-03-30 [2] CRAN (R 4.2.0)
    ##  modelr          0.1.9   2022-08-19 [2] CRAN (R 4.2.0)
    ##  munsell         0.5.0   2018-06-12 [2] CRAN (R 4.2.0)
    ##  pillar          1.9.0   2023-03-22 [2] CRAN (R 4.2.0)
    ##  pkgconfig       2.0.3   2019-09-22 [2] CRAN (R 4.2.0)
    ##  purrr         * 1.0.2   2023-08-10 [2] CRAN (R 4.2.0)
    ##  R6              2.5.1   2021-08-19 [2] CRAN (R 4.2.0)
    ##  readr         * 2.1.5   2024-01-10 [2] CRAN (R 4.2.0)
    ##  readxl          1.4.1   2022-08-17 [2] CRAN (R 4.2.0)
    ##  reprex          2.0.2   2022-08-17 [2] CRAN (R 4.2.0)
    ##  rlang           1.1.4   2024-06-04 [2] CRAN (R 4.2.0)
    ##  rmarkdown       2.28    2024-08-17 [2] CRAN (R 4.2.0)
    ##  rstudioapi      0.14    2022-08-22 [2] CRAN (R 4.2.0)
    ##  rvest           1.0.3   2022-08-19 [2] CRAN (R 4.2.0)
    ##  scales          1.2.1   2022-08-20 [2] CRAN (R 4.2.0)
    ##  sessioninfo     1.2.2   2021-12-06 [2] CRAN (R 4.2.0)
    ##  stringi         1.8.4   2024-05-06 [2] CRAN (R 4.2.0)
    ##  stringr       * 1.5.1   2023-11-14 [2] CRAN (R 4.2.0)
    ##  tibble        * 3.2.1   2023-03-20 [2] CRAN (R 4.2.0)
    ##  tidyr         * 1.3.0   2023-01-24 [2] CRAN (R 4.2.0)
    ##  tidyselect      1.2.1   2024-03-11 [2] CRAN (R 4.2.0)
    ##  tidyverse     * 1.3.2   2022-07-18 [2] CRAN (R 4.2.0)
    ##  tzdb            0.4.0   2023-05-12 [2] CRAN (R 4.2.0)
    ##  utf8            1.2.4   2023-10-22 [2] CRAN (R 4.2.0)
    ##  vctrs           0.6.5   2023-12-01 [2] CRAN (R 4.2.0)
    ##  withr           3.0.0   2024-01-16 [2] CRAN (R 4.2.0)
    ##  xfun            0.47    2024-08-17 [2] CRAN (R 4.2.0)
    ##  xml2            1.3.3   2021-11-30 [2] CRAN (R 4.2.0)
    ##  yaml            2.3.5   2022-02-21 [2] CRAN (R 4.2.0)
    ## 
    ##  [1] /home/sophieennis/R/x86_64-pc-linux-gnu-library/4.2
    ##  [2] /opt/R/4.2.0/lib/R/library
    ## 
    ## ──────────────────────────────────────────────────────────────────────────────
