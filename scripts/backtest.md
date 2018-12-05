Setup
-----

R is single-threated by default so I’m setting up a working cluster here
to access computational power of all available cores on computer.

``` r
library(magrittr)

knitr::opts_chunk$set(echo = T, cache = T, comment = "#>")

cluster <- multidplyr::create_cluster(parallel:::detectCores())
```

Load
----

I load the data into two dataframes here; one for historical data
(“data\_price\_etf.csv” in the original github repo) and one for
qualitative data (“data\_static\_etf\_com.csv” in the original github
repo).  
I limit the analysis to 100 randomly picked names here to keep things
light as long as developping the code. Will run on the whole dataset
when everything is fully developped and works well.

``` r
random_names <- names(readr::read_csv(file = "../data/data_historic_etf.csv", n_max = 1L))[-1L]
random_names <- random_names[sample.int(NROW(random_names), 100L)]

historic <- readr::read_csv(file = "../data/data_historic_etf.csv") %>%
  dplyr::mutate(date = as.Date(Date, origin = "1970-01-01")) %>% dplyr::arrange(date) %>%
  dplyr::select(-Date) %>% tidyr::gather(`name`, price, -date) %>% 
  dplyr::mutate(price = as.numeric(price)) %>% dplyr::filter(complete.cases(.)) %>%
  dplyr::filter(name %in% random_names)
static <- readr::read_csv(file = "../data/data_static_etf.csv") %>%
  dplyr::rename(name = ticker) %>% tidyr::gather(field, value, -name) %>%
  dplyr::mutate(field = forcats::as_factor(gsub(x = field, pattern = "_", replacement = "."))) %>%
  tidyr::spread(field, value) %>%
  dplyr::filter(name %in% random_names)
horizons <- c(4L, 8L, 13L, 26L, 52L)
multidplyr::cluster_assign_value(cluster, "horizons", horizons)
```

Transform
---------

To my understanding, feature engineering here involves calculating
statistics as well as constructing TDA features at regular interval
(monthly) from the time series data at regular interval (monthly). The
resulting dataframe would contain, for each month, one sample per name
(ETF) where features include time series statistics, TDA values as well
as qualitative information (static dataframe).

### Historic features

I understand that time series statistics include return, high minus low
as well as volatility over various time horizons including past week,
past 4 weeks, past 8 weeks, past 13 weeks, past 26 weeks, past 52 weeks.

#### Returns

I calculate returns as the relative change in price over the
above-mentioned time horizons here.

``` r
returns <- historic %>% dplyr::group_by(`name`) %>%
  dplyr::do({data <- .; data.table::rbindlist(
    lapply(c(1L, horizons), function(x)
      tibble::tibble(value = data$price / dplyr::lag(data$price, n = x) - 1L,
                     date = data$date, 
                     field = paste("return", x, ifelse(x > 1L, "weeks", "week"), sep = "."))
    ))}) %>% dplyr::select(name, field, date, value) %>% dplyr::filter(! is.na(value)) %>% 
  dplyr::mutate(field = forcats::as_factor(field)) %>% tidyr::spread(field, value)
```

#### High - low

I calculate high minus low by substracting the minimum observed value
from the maximum observed value over the above-mentioned time horizons
here.

``` r
HmL <- function(x) max(x) - min(x)
multidplyr::cluster_assign_value(cluster, "HmL", HmL)

HmL <- historic %>% multidplyr::partition(name, cluster = cluster) %>%
  dplyr::do({data <- .; data.table::rbindlist(
    lapply(horizons, function(x)
      tibble::tibble(value = tryCatch(tibbletime::rollify(HmL, window = x)(data$price),
                                      error = function(e) NA),
                     date = data$date, 
                     field = paste("HmL", x, "weeks", sep = "."))
    ))}) %>% dplyr::collect() %>% dplyr::select(name, field, date, value) %>%
  dplyr::filter(! is.na(value)) %>% dplyr::mutate(field = forcats::as_factor(field)) %>%
  tidyr::spread(field, value)
```

#### Volatility

I calculate volatility as the annualized standard deviation of 1-week
returns over the above-mentioned time horizons here.

``` r
volatility <- function(x) sd(x, na.rm = T) * sqrt(52L)
multidplyr::cluster_assign_value(cluster, "volatility", volatility)

volatility <- dplyr::select(returns, name, date, return = return.1.week) %>%
  multidplyr::partition(name, cluster = cluster) %>%
  dplyr::do({data <- .; data.table::rbindlist(
    lapply(horizons, function(x)
      tibble::tibble(value = tryCatch(tibbletime::rollify(volatility, window = x)(data$return),
                                      error = function(e) NA),
                     date = data$date, 
                     field = paste("volatility", x, "weeks", sep = "."))
    ))}) %>% dplyr::collect() %>% dplyr::select(name, field, date, value) %>%
  dplyr::filter(! is.na(value)) %>% dplyr::mutate(field = forcats::as_factor(field)) %>% 
  tidyr::spread(field, value)
```

#### TDA

Ok, got it, see bellow if things look alright to you.

``` r
TDA <- function(x) {
  diag <- TDA::gridDiag(FUNvalues = x ,sublevel = F, printProgress = F)$diagram
  values <- seq(min(diag[, c("Death", "Birth")]), max(diag[, c("Death", "Birth")]), 
                length = 50L)
  out <- c(TDA::landscape(diag, dimension = 0L, KK = 2L, tseq = values)[, 1L],
           TDA::landscape(diag, dimension = 0L, KK = 3L, tseq = values)[, 1L])
  
  magrittr::set_names(as.list(out), paste("TDA", 1L:100L))
}
multidplyr::cluster_assign_value(cluster, "TDA", TDA)

TDA <- historic %>% multidplyr::partition(name, cluster = cluster) %>%
  dplyr::do({ data <- .; 
  tryCatch(
    data.table::rbindlist(
      lapply(52L:nrow(data), function(x)
        cbind(date = dplyr::filter(data, dplyr::row_number() == x)$date,
              data.frame(TDA(dplyr::slice(data, (x - 51L):x)$price))))
    ),
    error = function(e) 
      data.frame(magrittr::set_names(as.list(rep(NA, 101L)), c("date", paste("TDA", 1L:100L))))
    )
  }) %>% dplyr::collect()
```

#### Wrap up

``` r
data <- Reduce(function(x, y) merge(x, y, by = c("name", "date"), all = T),
               list(returns, HmL, volatility, TDA)) %>%
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
  dplyr::group_by(name, year, month) %>% dplyr::filter(dplyr::row_number() == n()) %>% 
  dplyr::group_by(name) %>% dplyr::slice(52L:n()) %>% dplyr::ungroup() %>%
  dplyr::select(-c("year", "month")) %>% dplyr::left_join(static, by = "name")
data
```

    #> # A tibble: 4,090 x 135
    #>    name  date       return.1.week return.4.weeks return.8.weeks
    #>    <chr> <date>             <dbl>          <dbl>          <dbl>
    #>  1 ADRU  2011-08-26       0.0250        -0.127        -0.164   
    #>  2 ADRU  2011-09-30       0.0296        -0.0738       -0.103   
    #>  3 ADRU  2011-10-28       0.0576         0.166         0.0801  
    #>  4 ADRU  2011-11-25      -0.0612        -0.145        -0.00345 
    #>  5 ADRU  2011-12-30       0.00492       -0.0106       -0.0362  
    #>  6 ADRU  2012-01-27       0.00937        0.0403        0.0292  
    #>  7 ADRU  2012-02-24       0.0196         0.0454        0.0875  
    #>  8 ADRU  2012-03-30      -0.0154        -0.0179       -0.000909
    #>  9 ADRU  2012-04-27       0.0115        -0.00859      -0.0263  
    #> 10 ADRU  2012-05-25       0.00187       -0.0952       -0.103   
    #> # ... with 4,080 more rows, and 130 more variables: return.13.weeks <dbl>,
    #> #   return.26.weeks <dbl>, return.52.weeks <dbl>, HmL.4.weeks <dbl>,
    #> #   HmL.8.weeks <dbl>, HmL.13.weeks <dbl>, HmL.26.weeks <dbl>,
    #> #   HmL.52.weeks <dbl>, volatility.4.weeks <dbl>,
    #> #   volatility.8.weeks <dbl>, volatility.13.weeks <dbl>,
    #> #   volatility.26.weeks <dbl>, volatility.52.weeks <dbl>, TDA.1 <dbl>,
    #> #   TDA.2 <dbl>, TDA.3 <dbl>, TDA.4 <dbl>, TDA.5 <dbl>, TDA.6 <dbl>,
    #> #   TDA.7 <dbl>, TDA.8 <dbl>, TDA.9 <dbl>, TDA.10 <dbl>, TDA.11 <dbl>,
    #> #   TDA.12 <dbl>, TDA.13 <dbl>, TDA.14 <dbl>, TDA.15 <dbl>, TDA.16 <dbl>,
    #> #   TDA.17 <dbl>, TDA.18 <dbl>, TDA.19 <dbl>, TDA.20 <dbl>, TDA.21 <dbl>,
    #> #   TDA.22 <dbl>, TDA.23 <dbl>, TDA.24 <dbl>, TDA.25 <dbl>, TDA.26 <dbl>,
    #> #   TDA.27 <dbl>, TDA.28 <dbl>, TDA.29 <dbl>, TDA.30 <dbl>, TDA.31 <dbl>,
    #> #   TDA.32 <dbl>, TDA.33 <dbl>, TDA.34 <dbl>, TDA.35 <dbl>, TDA.36 <dbl>,
    #> #   TDA.37 <dbl>, TDA.38 <dbl>, TDA.39 <dbl>, TDA.40 <dbl>, TDA.41 <dbl>,
    #> #   TDA.42 <dbl>, TDA.43 <dbl>, TDA.44 <dbl>, TDA.45 <dbl>, TDA.46 <dbl>,
    #> #   TDA.47 <dbl>, TDA.48 <dbl>, TDA.49 <dbl>, TDA.50 <dbl>, TDA.51 <dbl>,
    #> #   TDA.52 <dbl>, TDA.53 <dbl>, TDA.54 <dbl>, TDA.55 <dbl>, TDA.56 <dbl>,
    #> #   TDA.57 <dbl>, TDA.58 <dbl>, TDA.59 <dbl>, TDA.60 <dbl>, TDA.61 <dbl>,
    #> #   TDA.62 <dbl>, TDA.63 <dbl>, TDA.64 <dbl>, TDA.65 <dbl>, TDA.66 <dbl>,
    #> #   TDA.67 <dbl>, TDA.68 <dbl>, TDA.69 <dbl>, TDA.70 <dbl>, TDA.71 <dbl>,
    #> #   TDA.72 <dbl>, TDA.73 <dbl>, TDA.74 <dbl>, TDA.75 <dbl>, TDA.76 <dbl>,
    #> #   TDA.77 <dbl>, TDA.78 <dbl>, TDA.79 <dbl>, TDA.80 <dbl>, TDA.81 <dbl>,
    #> #   TDA.82 <dbl>, TDA.83 <dbl>, TDA.84 <dbl>, TDA.85 <dbl>, TDA.86 <dbl>,
    #> #   TDA.87 <dbl>, ...

### Merge with qualitative features dataset

And there is the features dataset; does everything looks alright to you?

``` r
data %<>% dplyr::left_join(static, by = "name")
data
```

    #> # A tibble: 4,090 x 152
    #>    name  date       return.1.week return.4.weeks return.8.weeks
    #>    <chr> <date>             <dbl>          <dbl>          <dbl>
    #>  1 ADRU  2011-08-26       0.0250        -0.127        -0.164   
    #>  2 ADRU  2011-09-30       0.0296        -0.0738       -0.103   
    #>  3 ADRU  2011-10-28       0.0576         0.166         0.0801  
    #>  4 ADRU  2011-11-25      -0.0612        -0.145        -0.00345 
    #>  5 ADRU  2011-12-30       0.00492       -0.0106       -0.0362  
    #>  6 ADRU  2012-01-27       0.00937        0.0403        0.0292  
    #>  7 ADRU  2012-02-24       0.0196         0.0454        0.0875  
    #>  8 ADRU  2012-03-30      -0.0154        -0.0179       -0.000909
    #>  9 ADRU  2012-04-27       0.0115        -0.00859      -0.0263  
    #> 10 ADRU  2012-05-25       0.00187       -0.0952       -0.103   
    #> # ... with 4,080 more rows, and 147 more variables: return.13.weeks <dbl>,
    #> #   return.26.weeks <dbl>, return.52.weeks <dbl>, HmL.4.weeks <dbl>,
    #> #   HmL.8.weeks <dbl>, HmL.13.weeks <dbl>, HmL.26.weeks <dbl>,
    #> #   HmL.52.weeks <dbl>, volatility.4.weeks <dbl>,
    #> #   volatility.8.weeks <dbl>, volatility.13.weeks <dbl>,
    #> #   volatility.26.weeks <dbl>, volatility.52.weeks <dbl>, TDA.1 <dbl>,
    #> #   TDA.2 <dbl>, TDA.3 <dbl>, TDA.4 <dbl>, TDA.5 <dbl>, TDA.6 <dbl>,
    #> #   TDA.7 <dbl>, TDA.8 <dbl>, TDA.9 <dbl>, TDA.10 <dbl>, TDA.11 <dbl>,
    #> #   TDA.12 <dbl>, TDA.13 <dbl>, TDA.14 <dbl>, TDA.15 <dbl>, TDA.16 <dbl>,
    #> #   TDA.17 <dbl>, TDA.18 <dbl>, TDA.19 <dbl>, TDA.20 <dbl>, TDA.21 <dbl>,
    #> #   TDA.22 <dbl>, TDA.23 <dbl>, TDA.24 <dbl>, TDA.25 <dbl>, TDA.26 <dbl>,
    #> #   TDA.27 <dbl>, TDA.28 <dbl>, TDA.29 <dbl>, TDA.30 <dbl>, TDA.31 <dbl>,
    #> #   TDA.32 <dbl>, TDA.33 <dbl>, TDA.34 <dbl>, TDA.35 <dbl>, TDA.36 <dbl>,
    #> #   TDA.37 <dbl>, TDA.38 <dbl>, TDA.39 <dbl>, TDA.40 <dbl>, TDA.41 <dbl>,
    #> #   TDA.42 <dbl>, TDA.43 <dbl>, TDA.44 <dbl>, TDA.45 <dbl>, TDA.46 <dbl>,
    #> #   TDA.47 <dbl>, TDA.48 <dbl>, TDA.49 <dbl>, TDA.50 <dbl>, TDA.51 <dbl>,
    #> #   TDA.52 <dbl>, TDA.53 <dbl>, TDA.54 <dbl>, TDA.55 <dbl>, TDA.56 <dbl>,
    #> #   TDA.57 <dbl>, TDA.58 <dbl>, TDA.59 <dbl>, TDA.60 <dbl>, TDA.61 <dbl>,
    #> #   TDA.62 <dbl>, TDA.63 <dbl>, TDA.64 <dbl>, TDA.65 <dbl>, TDA.66 <dbl>,
    #> #   TDA.67 <dbl>, TDA.68 <dbl>, TDA.69 <dbl>, TDA.70 <dbl>, TDA.71 <dbl>,
    #> #   TDA.72 <dbl>, TDA.73 <dbl>, TDA.74 <dbl>, TDA.75 <dbl>, TDA.76 <dbl>,
    #> #   TDA.77 <dbl>, TDA.78 <dbl>, TDA.79 <dbl>, TDA.80 <dbl>, TDA.81 <dbl>,
    #> #   TDA.82 <dbl>, TDA.83 <dbl>, TDA.84 <dbl>, TDA.85 <dbl>, TDA.86 <dbl>,
    #> #   TDA.87 <dbl>, ...

Moving on to working on modeling part.
