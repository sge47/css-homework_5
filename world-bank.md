HW5 Part 2: Working with functions and tidyr with social science data
================
Sophie Ennis

## Overview

The World Bank publishes extensive socioeconomic data on countries and
economies worldwide. In the `data_world_bank` folder included in this
assignment, there are all the World Bank’s `csv` data files with
economic indicators for each country
(<https://data.worldbank.org/indicator>). Each `csv` file contains data
on a given country economy’s data.

Your tasks:

1.  Write a function that imports a data file and renames some of the
    columns. Then call the function to import all files.
2.  Tidy the imported data.
3.  Analyze a selection of the data.

Below are details for each of these tasks.

## Load necessary libraries

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

## Task 1

#### Write a function that imports the data files and renames columns

Your function should import a SINGLE data file and have one single
argument: the file path to the data file. Given this path (when you use
the function, pass a relative path), the function should import the
data, rename a few variables, and return the renamed data as output.

Your function should rename the following four variables “Country Name”,
“Country Code”, “Indicator Name”, “Indicator Code” as `country`,
`country_code`, `indicator`, `indicator_code`. Note the original
variables use non-syntactic names.

Your function must have documentation (see slides for examples).

Tips:

-   First, write code to import one file without a function (refer to
    lecture on importing/exporting data for more)
-   Inspect a few different `csv` files to familiarize yourself with the
    data and adjust your code accordingly: when you import the data you
    want to skip or drop problematic rows and columns, for example the
    first few rows and column 67 (you can refer to it as `...67`)
-   Once you are sure the code works correctly, put it into a function;
    see above on what the function is expected to do
-   Add documentation to your function (refer to the slides for
    examples)

``` r
library(readr)
read_csv(file = "data_world_bank/API_ABW_DS2_en_csv_v2_4346306.csv")
```

    ## New names:
    ## • `` -> `...3`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 1444 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Data Source, World Development Indicators, ...3
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## # A tibble: 1,444 × 3
    ##    `Data Source`     `World Development Indicators` ...3                        
    ##    <chr>             <chr>                          <chr>                       
    ##  1 Last Updated Date 2022-07-20                      <NA>                       
    ##  2 Country Name      Country Code                   "Indicator Name,Indicator C…
    ##  3 Aruba             ABW                            "Internally displaced perso…
    ##  4 Aruba             ABW                            "Merchandise exports to low…
    ##  5 Aruba             ABW                            "Net barter terms of trade …
    ##  6 Aruba             ABW                            "Merchandise imports from h…
    ##  7 Aruba             ABW                            "Share of tariff lines with…
    ##  8 Aruba             ABW                            "Share of tariff lines with…
    ##  9 Aruba             ABW                            "Urban population (% of tot…
    ## 10 Aruba             ABW                            "Population, total,SP.POP.T…
    ## # ℹ 1,434 more rows

``` r
read_csv(file = "data_world_bank/API_AFG_DS2_en_csv_v2_4343152.csv")
```

    ## New names:
    ## • `` -> `...3`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 1444 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Data Source, World Development Indicators, ...3
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## # A tibble: 1,444 × 3
    ##    `Data Source`     `World Development Indicators` ...3                        
    ##    <chr>             <chr>                          <chr>                       
    ##  1 Last Updated Date 2022-07-20                      <NA>                       
    ##  2 Country Name      Country Code                   "Indicator Name,Indicator C…
    ##  3 Afghanistan       AFG                            "Internally displaced perso…
    ##  4 Afghanistan       AFG                            "Merchandise exports to low…
    ##  5 Afghanistan       AFG                            "Net barter terms of trade …
    ##  6 Afghanistan       AFG                            "Merchandise imports from h…
    ##  7 Afghanistan       AFG                            "Share of tariff lines with…
    ##  8 Afghanistan       AFG                            "Share of tariff lines with…
    ##  9 Afghanistan       AFG                            "Urban population (% of tot…
    ## 10 Afghanistan       AFG                            "Population, total,SP.POP.T…
    ## # ℹ 1,434 more rows

``` r
read_rename <- function(df) {
  # Reads a single file and renames its columns
  # Args:
    # df(tibble): of input data
  # Returns: 
    # data from file with renamed columns
  data <- read_csv(file = df, skip = 4)
    if ("...67" %in% colnames(data)) {
    data_clean <- data %>% select(-"...67")
  }
  renamed_data <- data_clean %>% 
    rename(
           country = `Country Name`,
           country_code = `Country Code`,
           indicator = `Indicator Name`,
           indicator_code = `Indicator Code`
           )
  return(renamed_data)
}
read_rename("data_world_bank/API_ABW_DS2_en_csv_v2_4346306.csv")
```

    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...67`

    ## # A tibble: 1,442 × 66
    ##    country country_code indicator indicator_code  `1960`  `1961`  `1962`  `1963`
    ##    <chr>   <chr>        <chr>     <chr>            <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Aruba   ABW          Internal… VC.IDP.NWCV    NA      NA      NA      NA     
    ##  2 Aruba   ABW          Merchand… TX.VAL.MRCH.R… NA      NA      NA      NA     
    ##  3 Aruba   ABW          Net bart… TT.PRI.MRCH.X… NA      NA      NA      NA     
    ##  4 Aruba   ABW          Merchand… TM.VAL.MRCH.H… NA      NA      NA      NA     
    ##  5 Aruba   ABW          Share of… TM.TAX.TCOM.I… NA      NA      NA      NA     
    ##  6 Aruba   ABW          Share of… TM.TAX.MANF.I… NA      NA      NA      NA     
    ##  7 Aruba   ABW          Urban po… SP.URB.TOTL.I…  5.08e1  5.08e1  5.07e1  5.07e1
    ##  8 Aruba   ABW          Populati… SP.POP.TOTL     5.42e4  5.54e4  5.62e4  5.67e4
    ##  9 Aruba   ABW          Populati… SP.POP.65UP.M…  2.03e0  2.13e0  2.26e0  2.36e0
    ## 10 Aruba   ABW          Populati… SP.POP.3539.M…  5.20e0  5.11e0  5.02e0  4.94e0
    ## # ℹ 1,432 more rows
    ## # ℹ 58 more variables: `1964` <dbl>, `1965` <dbl>, `1966` <dbl>, `1967` <dbl>,
    ## #   `1968` <dbl>, `1969` <dbl>, `1970` <dbl>, `1971` <dbl>, `1972` <dbl>,
    ## #   `1973` <dbl>, `1974` <dbl>, `1975` <dbl>, `1976` <dbl>, `1977` <dbl>,
    ## #   `1978` <dbl>, `1979` <dbl>, `1980` <dbl>, `1981` <dbl>, `1982` <dbl>,
    ## #   `1983` <dbl>, `1984` <dbl>, `1985` <dbl>, `1986` <dbl>, `1987` <dbl>,
    ## #   `1988` <dbl>, `1989` <dbl>, `1990` <dbl>, `1991` <dbl>, `1992` <dbl>, …

#### Call the function to import all data files

Once you are sure your function works as expected to import one SINGLE
data file, use it to import ALL data files. To do so:

-   First, create a list that stores the names of all data files using
    the `dir()` function, with the following three arguments (type
    `help("dir")` in your console for more info about this function):

    -   `path`: use a relative path (shorter, relative to your working
        directory) rather than an absolute path (longer, more prone to
        errors)
    -   `pattern`: use a simple regular expression pattern that matches
        the `csv` extension of all your files (for example `"csv$"` for
        more on this topic see the regular expression lecture)
    -   `full.names`: set this to `TRUE`

-   Second, import all data files using your function. Demonstrate how
    to do this in two ways: using a `for loop` and using the most
    appropriate `map` function. Note that since there are several files,
    the import process might take a while, especially when using the
    loop (up to 2-3 minutes, depending on your machine).

-   Ensure that your final result is a single dataframe (both when using
    a `for loop` and the `map` function), not a list or any other data
    structure. Print the first few rows of the final dataframe.

``` r
# your code to create a list with the names of all data files to import; do not print the entire list in your submitted homework (printing only the first few names is enough)
all_data <- dir(
  path = "data_world_bank/", pattern = "API_",
           full.names = TRUE)
head(all_data)
```

    ## [1] "data_world_bank//API_ABW_DS2_en_csv_v2_4346306.csv"
    ## [2] "data_world_bank//API_AFG_DS2_en_csv_v2_4343152.csv"
    ## [3] "data_world_bank//API_AGO_DS2_en_csv_v2_4319971.csv"
    ## [4] "data_world_bank//API_ALB_DS2_en_csv_v2_4331460.csv"
    ## [5] "data_world_bank//API_AND_DS2_en_csv_v2_4346430.csv"
    ## [6] "data_world_bank//API_ARE_DS2_en_csv_v2_4335014.csv"

``` r
# your code to import all data files using a for loop
all_renamed_data <- vector("list", length = length(all_data))
for (i in seq_along(all_data)) {
  all_renamed_data[[i]] <- read_rename(all_data[[i]])
}
```

    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## • `` -> `...67`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 1442 Columns: 67
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): Country Name, Country Code, Indicator Name, Indicator Code
    ## dbl (61): 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, ...
    ## lgl  (2): 1960, ...67
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): Country Name, Country Code, Indicator Name, Indicator Code
    ## dbl (62): 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ...
    ## lgl  (1): ...67
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): Country Name, Country Code, Indicator Name, Indicator Code
    ## dbl (62): 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ...
    ## lgl  (1): ...67
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): Country Name, Country Code, Indicator Name, Indicator Code
    ## dbl (62): 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ...
    ## lgl  (1): ...67
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): Country Name, Country Code, Indicator Name, Indicator Code
    ## dbl (62): 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ...
    ## lgl  (1): ...67
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
combined_data <- bind_rows(all_renamed_data)
tail(combined_data)
```

    ## # A tibble: 6 × 66
    ##   country  country_code indicator  indicator_code `1960`  `1961`  `1962`  `1963`
    ##   <chr>    <chr>        <chr>      <chr>           <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Zimbabwe ZWE          Net bilat… DC.DAC.CANL.CD     NA NA      NA      NA     
    ## 2 Zimbabwe ZWE          Foreign d… BX.KLT.DINV.W…     NA NA      NA      NA     
    ## 3 Zimbabwe ZWE          Net capit… BN.TRF.KOGT.CD     NA NA      NA      NA     
    ## 4 Zimbabwe ZWE          Travel se… BM.GSR.TRVL.ZS     NA NA      NA      NA     
    ## 5 Zimbabwe ZWE          Cereal pr… AG.PRD.CREL.MT     NA  1.27e6  1.23e6  1.03e6
    ## 6 Zimbabwe ZWE          Arable la… AG.LND.ARBL.ZS     NA  4.87e0  5.00e0  5.13e0
    ## # ℹ 58 more variables: `1964` <dbl>, `1965` <dbl>, `1966` <dbl>, `1967` <dbl>,
    ## #   `1968` <dbl>, `1969` <dbl>, `1970` <dbl>, `1971` <dbl>, `1972` <dbl>,
    ## #   `1973` <dbl>, `1974` <dbl>, `1975` <dbl>, `1976` <dbl>, `1977` <dbl>,
    ## #   `1978` <dbl>, `1979` <dbl>, `1980` <dbl>, `1981` <dbl>, `1982` <dbl>,
    ## #   `1983` <dbl>, `1984` <dbl>, `1985` <dbl>, `1986` <dbl>, `1987` <dbl>,
    ## #   `1988` <dbl>, `1989` <dbl>, `1990` <dbl>, `1991` <dbl>, `1992` <dbl>,
    ## #   `1993` <dbl>, `1994` <dbl>, `1995` <dbl>, `1996` <dbl>, `1997` <dbl>, …

``` r
head(combined_data)
```

    ## # A tibble: 6 × 66
    ##   country country_code indicator      indicator_code `1960` `1961` `1962` `1963`
    ##   <chr>   <chr>        <chr>          <chr>           <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 Aruba   ABW          Internally di… VC.IDP.NWCV        NA     NA     NA     NA
    ## 2 Aruba   ABW          Merchandise e… TX.VAL.MRCH.R…     NA     NA     NA     NA
    ## 3 Aruba   ABW          Net barter te… TT.PRI.MRCH.X…     NA     NA     NA     NA
    ## 4 Aruba   ABW          Merchandise i… TM.VAL.MRCH.H…     NA     NA     NA     NA
    ## 5 Aruba   ABW          Share of tari… TM.TAX.TCOM.I…     NA     NA     NA     NA
    ## 6 Aruba   ABW          Share of tari… TM.TAX.MANF.I…     NA     NA     NA     NA
    ## # ℹ 58 more variables: `1964` <dbl>, `1965` <dbl>, `1966` <dbl>, `1967` <dbl>,
    ## #   `1968` <dbl>, `1969` <dbl>, `1970` <dbl>, `1971` <dbl>, `1972` <dbl>,
    ## #   `1973` <dbl>, `1974` <dbl>, `1975` <dbl>, `1976` <dbl>, `1977` <dbl>,
    ## #   `1978` <dbl>, `1979` <dbl>, `1980` <dbl>, `1981` <dbl>, `1982` <dbl>,
    ## #   `1983` <dbl>, `1984` <dbl>, `1985` <dbl>, `1986` <dbl>, `1987` <dbl>,
    ## #   `1988` <dbl>, `1989` <dbl>, `1990` <dbl>, `1991` <dbl>, `1992` <dbl>,
    ## #   `1993` <dbl>, `1994` <dbl>, `1995` <dbl>, `1996` <dbl>, `1997` <dbl>, …

``` r
nrow(combined_data)
```

    ## [1] 312914

``` r
is.data.frame(combined_data)
```

    ## [1] TRUE

``` r
# your code to import all data files using map
library(purrr)

map_df(all_data, read_rename)
```

    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (4): Country Name, Country Code, Indicator Name, Indicator Code dbl (62): 1960,
    ## 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ... lgl (1): ...67
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## • `` -> `...67`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 1442 Columns: 67
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): Country Name, Country Code, Indicator Name, Indicator Code
    ## dbl (61): 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, ...
    ## lgl  (2): 1960, ...67
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): Country Name, Country Code, Indicator Name, Indicator Code
    ## dbl (62): 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ...
    ## lgl  (1): ...67
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): Country Name, Country Code, Indicator Name, Indicator Code
    ## dbl (62): 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ...
    ## lgl  (1): ...67
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): Country Name, Country Code, Indicator Name, Indicator Code
    ## dbl (62): 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ...
    ## lgl  (1): ...67
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## New names:
    ## Rows: 1442 Columns: 67
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): Country Name, Country Code, Indicator Name, Indicator Code
    ## dbl (62): 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, ...
    ## lgl  (1): ...67
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## # A tibble: 312,914 × 66
    ##    country country_code indicator indicator_code  `1960`  `1961`  `1962`  `1963`
    ##    <chr>   <chr>        <chr>     <chr>            <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Aruba   ABW          Internal… VC.IDP.NWCV    NA      NA      NA      NA     
    ##  2 Aruba   ABW          Merchand… TX.VAL.MRCH.R… NA      NA      NA      NA     
    ##  3 Aruba   ABW          Net bart… TT.PRI.MRCH.X… NA      NA      NA      NA     
    ##  4 Aruba   ABW          Merchand… TM.VAL.MRCH.H… NA      NA      NA      NA     
    ##  5 Aruba   ABW          Share of… TM.TAX.TCOM.I… NA      NA      NA      NA     
    ##  6 Aruba   ABW          Share of… TM.TAX.MANF.I… NA      NA      NA      NA     
    ##  7 Aruba   ABW          Urban po… SP.URB.TOTL.I…  5.08e1  5.08e1  5.07e1  5.07e1
    ##  8 Aruba   ABW          Populati… SP.POP.TOTL     5.42e4  5.54e4  5.62e4  5.67e4
    ##  9 Aruba   ABW          Populati… SP.POP.65UP.M…  2.03e0  2.13e0  2.26e0  2.36e0
    ## 10 Aruba   ABW          Populati… SP.POP.3539.M…  5.20e0  5.11e0  5.02e0  4.94e0
    ## # ℹ 312,904 more rows
    ## # ℹ 58 more variables: `1964` <dbl>, `1965` <dbl>, `1966` <dbl>, `1967` <dbl>,
    ## #   `1968` <dbl>, `1969` <dbl>, `1970` <dbl>, `1971` <dbl>, `1972` <dbl>,
    ## #   `1973` <dbl>, `1974` <dbl>, `1975` <dbl>, `1976` <dbl>, `1977` <dbl>,
    ## #   `1978` <dbl>, `1979` <dbl>, `1980` <dbl>, `1981` <dbl>, `1982` <dbl>,
    ## #   `1983` <dbl>, `1984` <dbl>, `1985` <dbl>, `1986` <dbl>, `1987` <dbl>,
    ## #   `1988` <dbl>, `1989` <dbl>, `1990` <dbl>, `1991` <dbl>, `1992` <dbl>, …

## Task 2

Tidy the imported data using the principles of tidy data: each variable
must have its own column, each observation must have its own row, each
value must have its own cell.

Before writing any code, take some time to envision how you want your
data to look like once tidy. In a few sentences, explain what you did
and why the resulting data frame is tidy.

``` r
tidy_data <- combined_data %>% 
  pivot_longer(
    cols = starts_with("19")|starts_with("20"),
    names_to = "year",
    values_to = "value",
    names_transform = list(year = as.integer)
  ) %>% 
  pivot_wider(
    id_cols = c(country, country_code, year),
    names_from = indicator,
    values_from = value
  )
head(tidy_data)
```

    ## # A tibble: 6 × 1,445
    ##   country country_code  year Internally displaced perso…¹ Merchandise exports …²
    ##   <chr>   <chr>        <int>                        <dbl>                  <dbl>
    ## 1 Aruba   ABW           1960                           NA                     NA
    ## 2 Aruba   ABW           1961                           NA                     NA
    ## 3 Aruba   ABW           1962                           NA                     NA
    ## 4 Aruba   ABW           1963                           NA                     NA
    ## 5 Aruba   ABW           1964                           NA                     NA
    ## 6 Aruba   ABW           1965                           NA                     NA
    ## # ℹ abbreviated names:
    ## #   ¹​`Internally displaced persons, new displacement associated with conflict and violence (number of cases)`,
    ## #   ²​`Merchandise exports to low- and middle-income economies in Europe & Central Asia (% of total merchandise exports)`
    ## # ℹ 1,440 more variables: `Net barter terms of trade index (2000 = 100)` <dbl>,
    ## #   `Merchandise imports from high-income economies (% of total merchandise imports)` <dbl>,
    ## #   `Share of tariff lines with international peaks, primary products (%)` <dbl>,
    ## #   `Share of tariff lines with international peaks, manufactured products (%)` <dbl>, …

The years were all columns and the indicator codes were all observations
in one column. I created one column for the years so that each
observation had its own row within a column. I did this by using
`pivot_longer()` and allocating all of the columns that started with 19
or 20, as the years in the dataset are the 1900s-2000s, to one column
named year. I then created a column for each of the indicator types,
dropping the indicator type column, so that each variable had its own
column. I did this by using `id_cols` as an argument in `pivot_wider()`
to specify which columns should be kept as identifiers and not put into
multiple columns.

## Task 3

After importing the data, write a brief report (one to two paragraphs)
analyzing the relationship between at least two variables of your choice
from the dataset.

Produce at least one graph that illustrates the relationship between
these variables. Ensure the graph has properly labeled axes, an
informative title, and any other elements needed for clarity.

Compute and report some descriptive statistics for the variables (e.g.,
mean, standard deviation, count, etc.).

If the variables require data wrangling (e.g., aggregation, recoding,
etc.) before plotting, please include the relevant code for these steps.

To familiarize yourself with the World Bank indicators, you may find the
data documentation helpful: <https://data.worldbank.org/indicator>

``` r
tidy_data <- tidy_data %>% 
  filter(!is.na(`Lifetime risk of maternal death (%)`)) %>% 
  mutate(
    maternal_death = case_when(
      `Lifetime risk of maternal death (%)` > 5.0 ~ "high",
      `Lifetime risk of maternal death (%)` >= 1.0 & 
      `Lifetime risk of maternal death (%)` <= 5.0 ~ "medium",
      `Lifetime risk of maternal death (%)` < 1.0 ~ "low",
      TRUE ~ NA_character_
    )
  ) 

avg_maternal_death <- tidy_data %>%
  group_by(maternal_death) %>%
  summarize(
    avg_maternal_death = mean(`Lifetime risk of maternal death (%)`, na.rm = TRUE)) %>% 
    arrange(desc(avg_maternal_death))
avg_maternal_death
```

    ## # A tibble: 3 × 2
    ##   maternal_death avg_maternal_death
    ##   <chr>                       <dbl>
    ## 1 high                        6.80 
    ## 2 medium                      2.60 
    ## 3 low                         0.172

``` r
tidy_data <- tidy_data %>% 
  filter(!is.na(`Fertility rate, total (births per woman)`)) %>% 
  mutate(
    fertility_rate = case_when(
      `Fertility rate, total (births per woman)` > 5.0 ~ "high",
      `Fertility rate, total (births per woman)` >= 3.0 & 
      `Fertility rate, total (births per woman)` <= 5.0 ~ "medium",
      `Fertility rate, total (births per woman)` < 3.0 ~ "low",
      TRUE ~ NA_character_
    )
  )

avg_fertility_rate <- tidy_data %>%
  group_by(fertility_rate) %>%
  summarize(
    avg_fertility_rate = mean(`Fertility rate, total (births per woman)`, na.rm = TRUE)) %>% 
    arrange(desc(avg_fertility_rate))
avg_fertility_rate
```

    ## # A tibble: 3 × 2
    ##   fertility_rate avg_fertility_rate
    ##   <chr>                       <dbl>
    ## 1 high                         5.85
    ## 2 medium                       3.99
    ## 3 low                          1.97

``` r
tidy_data$maternal_death <- factor(tidy_data$maternal_death, levels = c("high", "medium", "low"))
tidy_data$fertility_rate <- factor(tidy_data$fertility_rate, levels = c("high", "medium", "low"))

levels(tidy_data$maternal_death)
```

    ## [1] "high"   "medium" "low"

``` r
levels(tidy_data$fertility_rate)
```

    ## [1] "high"   "medium" "low"

``` r
ggplot(data = tidy_data,
       mapping = aes(
         x = maternal_death,
         fill = fertility_rate)) + 
  geom_bar(position = "stack") +
  labs(title = "Fertility Rate and Maternal Mortality Risk",
       x = "Lifetime Maternal Mortality Risk",
       y = "Cases")
```

![](world-bank_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
tidy_data_subset <- tidy_data %>% 
  filter(country %in% c("United States", "India", "Niger"))
ggplot(data = tidy_data_subset,
       mapping = aes(
         x = maternal_death,
         fill = fertility_rate)) + 
  geom_bar(position = "stack") +
  facet_wrap(~ country) +
  labs(title = "Fertility Rate and Maternal Mortality Risk",
       x = "Lifetime Maternal Mortality Risk",
       y = "Cases")
```

![](world-bank_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
tidy_data_subset <- tidy_data %>% 
  filter(country %in% c("United States", "India", "Niger"))

ggplot(data = tidy_data_subset,
       mapping = aes(
         x = maternal_death,
         y = `Population, total`,
         fill = fertility_rate)) + 
  geom_col(position = "stack") +
  facet_wrap(~ country) +
  labs(title = "Fertility Rate and Maternal Mortality Risk",
       x = "Lifetime Maternal Mortality Risk",
       y = "Total Population")
```

![](world-bank_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
tidy_data_subset <- tidy_data %>% 
  filter(country %in% c("United States", "India", "Niger")) %>% 
  group_by(country, maternal_death, fertility_rate) %>%
  summarize(avg_births_attended = mean(`Births attended by skilled health staff (% of total)`,
                                       na.rm = TRUE), .groups = 'drop')

ggplot(data = tidy_data_subset,
       mapping = aes(
         x = maternal_death,
         y = avg_births_attended,
         fill = fertility_rate)) + 
  geom_col(position = "stack") +
  facet_wrap(~ country) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Fertility Rate and Maternal Mortality Risk",
       x = "Lifetime Maternal Mortality Risk",
       y = "Average births attended by skilled health staff (% of total)")
```

    ## Warning: Removed 1 rows containing missing values (`position_stack()`).

![](world-bank_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

I wanted to assess if lifetime maternal mortality risk was associated
with fertility rates. I hypothesized that countries with higher
fertility rates would be associated with higher maternal mortality risk,
because the act of birthing children can be dangerous in itself,
particularly when compounded by the countries that have higher fertility
rates typically being more developing in nature. I generated four graphs
to illustrate this.

The first bar graph depicts the relationship between maternal mortality
risk and fertility rate for all countries. The second bar graph depicts
the same relationship but for three types of countries - the United
States, a developed country with a low fertility rate, India, a
developed country with a medium fertility rate, and Niger, a developing
country with a high fertility rate. These countries differ based on the
medical advancements they have to save mothers’ lives when they are at
risk - while the United States and India offer advanced medical
treatments, Niger does not have as many resources to advance healthcare.
This makes Niger a country where the risk of maternal mortality is
higher than these more developed nations.

The third graph indicates the differences between these countries in
population. While India has a very large population, because of its
medical advancements, the majority of its maternal mortality risk can be
classified as low, including low and medium classifications of fertility
rate. Niger has a nearly negligible population in comparison with India
and the United States. That said, Niger’s high and medium maternal
mortality risk, despite its high fertility rates, is at least not
widespread, affecting a very small population. The United States, which
is entirely classified as having a low maternal mortality risk, is also
classified as having a low fertility rate. India, having a higher
fertility rate than the United States, has almost an equal amount of
cases that are medium in their maternal mortality risk to the entirety
of the United States’ low maternal mortality risk, revealing not only
how large the population is, but how India is doing a very good job
mitigating their medium fertility rate through healthcare interventions.
The fourth graph shows that out of these countries, the United States is
the most likely to have mothers’ births be monitored by skilled health
staff. India and Niger are not too far behind in their own averages, but
this can show a few things: the United States may be the furthest ahead
in safe deliveries which thus regulate maternal mortality, or the United
States could just be leading in the medicalization of birth. Deliveries
are much more likely to happen in hospitals in the United States,
whereas in other countries like Niger, either due to different norms or
resources, home births happen more frequently.

I conclude that my hypothesis is supported by the data: high maternal
mortality risk is associated with high fertility rate. This measure must
account for population size as well as availability of healthcare.

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
    ##  backports       1.4.1   2021-12-13 [2] CRAN (R 4.2.0)
    ##  bit             4.0.5   2022-11-15 [2] CRAN (R 4.2.0)
    ##  bit64           4.0.5   2020-08-30 [2] CRAN (R 4.2.0)
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
    ##  scales        * 1.2.1   2022-08-20 [2] CRAN (R 4.2.0)
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
    ##  vroom           1.6.5   2023-12-05 [2] CRAN (R 4.2.0)
    ##  withr           3.0.0   2024-01-16 [2] CRAN (R 4.2.0)
    ##  xfun            0.47    2024-08-17 [2] CRAN (R 4.2.0)
    ##  xml2            1.3.3   2021-11-30 [2] CRAN (R 4.2.0)
    ##  yaml            2.3.5   2022-02-21 [2] CRAN (R 4.2.0)
    ## 
    ##  [1] /home/sophieennis/R/x86_64-pc-linux-gnu-library/4.2
    ##  [2] /opt/R/4.2.0/lib/R/library
    ## 
    ## ──────────────────────────────────────────────────────────────────────────────

## Reflections

Write a few sentences reflecting on what was difficult or easy about
this homework assignment (evaluate both parts of this assigment:
debugging and word-bank data). Discuss the problems you encountered and
how you solved them, and new concepts or techniques you learned.

Please, list the first and last names of any collaborators you worked
with to complete this assignment. Additionally, specify the resources
you used, including how you utilized them. If you used AI, please
explain how (be specific!).

Make sure to check our policy for [Plagiarism and Academic
integrity](https://computing-soc-sci.netlify.app/faq/course-expectations/#plagiarism-and-academic-integrity)
including the use of AI. In short: do not copy large chunks of code from
the internet or generated by AI (that is plagiarism and we will penalize
it!), but using these tools to debug programs or overcome small problems
is acceptable, just make sure to explain how you use them (see above).

Finding and fixing the bugs in the `fix-errors.Rmd` Assignment went
pretty smoothly, and I found that working with `ggplot()` is a lot more
intuitive for me than writing functions. This difficulty writing
functions posed a slight challenge in the `world-bank.Rmd` Assignment
for Task 1, but Rosa and I were able to get through it. I found writing
the documentation for the function a little challenging but it really
challenged me to understand what the function I was writing was doing. I
found it slightly challenging to decide what and how to tidy about the
data, but once I played around with how many lines we should skip, we
found the right answer. I got TA Zach’s help with writing the for loop
in terms of what to pre-allocate, but the map function was more
straightforward. Professor Nardin’s comments on Ed really helped me
figure out what to do for Task 2 because after performing
`pivot_longer()` I was struggling with what to do next. Task 3 was
simultaneously frustrating and fun. I kept choosing variables that were
mostly comprised of `NA` values so the `summarize()` function kept
getting confused even when I dropped the `NA` values. I eventually found
variables of interest that had data present for most of the rows, and
from there, I recoded them using my knowledge of `case_when()` from
previous homework assignments pretty easily and then was able to
generate a bunch of graphs. My interpretation of the data was definitely
affected by my global knowledge, but I think I did a pretty good job
analyzing the graphs.

-   I worked with Rosa Lander on this assignment.
-   I used Professor Nardin’s advice posted on Ed on how to use
    `id_cols` to tidy the data for Task 2.
-   Zach’s office hours were especially helpful for debugging my code,
    particularly for Tasks 1 and 2.
-   I relied on the slides and the provided function documentation.
-   I used ChatGPT to debug Task 3, which recommended I transform the
    variables I was using into factors for better visualization.
