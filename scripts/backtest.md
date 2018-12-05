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
  dplyr::select(-c("year", "month"))
```

### Merge with qualitative features dataset

And there is the features dataset; does everything looks alright to you?

``` r
data %<>% dplyr::left_join(static, by = "name")
head(data)
```

    #> # A tibble: 6 x 135
    #>   name  date       return.1.week return.4.weeks return.8.weeks
    #>   <chr> <date>             <dbl>          <dbl>          <dbl>
    #> 1 ADZ   2012-07-27       0.0885        -0.0923         -0.168 
    #> 2 ADZ   2012-08-31      -0.0472        -0.00252        -0.0805
    #> 3 ADZ   2012-09-28       0.0165         0.0577          0.0550
    #> 4 ADZ   2012-10-26       0             -0.0306          0.0253
    #> 5 ADZ   2012-11-30      -0.00525        0.0286         -0.0137
    #> 6 ADZ   2012-12-28      -0.00987        0.0106          0.0395
    #> # ... with 130 more variables: return.13.weeks <dbl>,
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

``` r
names(data)
```

    #>   [1] "name"                "date"                "return.1.week"      
    #>   [4] "return.4.weeks"      "return.8.weeks"      "return.13.weeks"    
    #>   [7] "return.26.weeks"     "return.52.weeks"     "HmL.4.weeks"        
    #>  [10] "HmL.8.weeks"         "HmL.13.weeks"        "HmL.26.weeks"       
    #>  [13] "HmL.52.weeks"        "volatility.4.weeks"  "volatility.8.weeks" 
    #>  [16] "volatility.13.weeks" "volatility.26.weeks" "volatility.52.weeks"
    #>  [19] "TDA.1"               "TDA.2"               "TDA.3"              
    #>  [22] "TDA.4"               "TDA.5"               "TDA.6"              
    #>  [25] "TDA.7"               "TDA.8"               "TDA.9"              
    #>  [28] "TDA.10"              "TDA.11"              "TDA.12"             
    #>  [31] "TDA.13"              "TDA.14"              "TDA.15"             
    #>  [34] "TDA.16"              "TDA.17"              "TDA.18"             
    #>  [37] "TDA.19"              "TDA.20"              "TDA.21"             
    #>  [40] "TDA.22"              "TDA.23"              "TDA.24"             
    #>  [43] "TDA.25"              "TDA.26"              "TDA.27"             
    #>  [46] "TDA.28"              "TDA.29"              "TDA.30"             
    #>  [49] "TDA.31"              "TDA.32"              "TDA.33"             
    #>  [52] "TDA.34"              "TDA.35"              "TDA.36"             
    #>  [55] "TDA.37"              "TDA.38"              "TDA.39"             
    #>  [58] "TDA.40"              "TDA.41"              "TDA.42"             
    #>  [61] "TDA.43"              "TDA.44"              "TDA.45"             
    #>  [64] "TDA.46"              "TDA.47"              "TDA.48"             
    #>  [67] "TDA.49"              "TDA.50"              "TDA.51"             
    #>  [70] "TDA.52"              "TDA.53"              "TDA.54"             
    #>  [73] "TDA.55"              "TDA.56"              "TDA.57"             
    #>  [76] "TDA.58"              "TDA.59"              "TDA.60"             
    #>  [79] "TDA.61"              "TDA.62"              "TDA.63"             
    #>  [82] "TDA.64"              "TDA.65"              "TDA.66"             
    #>  [85] "TDA.67"              "TDA.68"              "TDA.69"             
    #>  [88] "TDA.70"              "TDA.71"              "TDA.72"             
    #>  [91] "TDA.73"              "TDA.74"              "TDA.75"             
    #>  [94] "TDA.76"              "TDA.77"              "TDA.78"             
    #>  [97] "TDA.79"              "TDA.80"              "TDA.81"             
    #> [100] "TDA.82"              "TDA.83"              "TDA.84"             
    #> [103] "TDA.85"              "TDA.86"              "TDA.87"             
    #> [106] "TDA.88"              "TDA.89"              "TDA.90"             
    #> [109] "TDA.91"              "TDA.92"              "TDA.93"             
    #> [112] "TDA.94"              "TDA.95"              "TDA.96"             
    #> [115] "TDA.97"              "TDA.98"              "TDA.99"             
    #> [118] "TDA.100"             "issuer"              "expense.ratio"      
    #> [121] "aum"                 "spread"              "asset.class"        
    #> [124] "strategy"            "region"              "category"           
    #> [127] "focus"               "niche"               "inverse"            
    #> [130] "leveraged"           "etn"                 "index.prov"         
    #> [133] "selection"           "weight.schm"         "act.p.sec"

Moving on to working on modeling part.
