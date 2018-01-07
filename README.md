
<!-- README.md is generated from README.Rmd. Please edit that file -->
killedbypolice
==============

The goal of killedbypolice is to make readily available the data collected by <http://killedbypolice.net/> for exploration, visualisation, and analysis.

Installation
------------

You can install killedbypolice from github with:

``` r
# install.packages("devtools")
devtools::install_github("benmarwick/killedbypolice")
```

Example
-------

This is a basic example which shows you how to access the data in this package:

``` r
# load the library
library(killedbypolice)

# load the data frame, ready to work with 
data("kbp2013_2017")

# inspect the data
str(kbp2013_2017)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    5578 obs. of  11 variables:
#>  $ date_format  : Date, format: "2013-12-31" "2013-12-31" ...
#>  $ State        : chr  "WA" "CA" "NC" "PA" ...
#>  $ deceased_name: chr  "James M. Eshelman" "Dontae Hayes" "Ricky Junior Toney" "William Jackson" ...
#>  $ deceased_age : num  54 20 34 43 31 43 45 22 30 51 ...
#>  $ gender       : chr  "M" "M" "M" "M" ...
#>  $ race         : chr  NA "B" "B" "B" ...
#>  $ method_1     : chr  "G" "G" "G" "G" ...
#>  $ method_2     : chr  NA NA NA NA ...
#>  $ method_3     : chr  NA NA NA NA ...
#>  $ method_4     : chr  NA NA NA NA ...
#>  $ method_5     : chr  NA NA NA NA ...
```
