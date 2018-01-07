
<!-- README.md is generated from README.Rmd. Please edit that file -->
killedbypolice
==============

The goal of killedbypolice is to make readily available the data collected by <http://killedbypolice.net/> for exploration, visualisation, and analysis.

This is an important data set because the 'US government has no comprehensive record of the number of people killed by law enforcement.' ([*The Guardian*, 1 June 2015](https://www.theguardian.com/us-news/ng-interactive/2015/jun/01/about-the-counted)). The killedbypolice project is one of a few non-government projects that continuously collect data on polic killings (see [Related work](#related-work) below).

Installation
------------

You can install killedbypolice from github with:

``` r
# install.packages("devtools")
devtools::install_github("benmarwick/killedbypolice")
```

How to use
----------

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

head(kbp2013_2017)
#>   date_format State      deceased_name deceased_age gender race method_1
#> 1  2013-12-31    WA  James M. Eshelman           54      M <NA>        G
#> 2  2013-12-31    CA       Dontae Hayes           20      M    B        G
#> 3  2013-12-31    NC Ricky Junior Toney           34      M    B        G
#> 4  2013-12-30    PA    William Jackson           43      M    B        G
#> 5  2013-12-30    CA      Bethany Lytle           31      F    W        G
#> 6  2013-12-29    CO Christopher George           43      M    W        G
#>   method_2 method_3 method_4 method_5
#> 1     <NA>     <NA>     <NA>     <NA>
#> 2     <NA>     <NA>     <NA>     <NA>
#> 3     <NA>     <NA>     <NA>     <NA>
#> 4     <NA>     <NA>     <NA>     <NA>
#> 5     <NA>     <NA>     <NA>     <NA>
#> 6     <NA>     <NA>     <NA>     <NA>
```

Some summaries
--------------

Here are some summaries of the data:

Related work
------------

We don't know much about who collects the data for <http://killedbypolice.net/>, or what their methods are. But there are several Python scripts for scraping it, for example by [markberger](https://github.com/markberger/police-killings-dataset) and [AceLewis](https://github.com/AceLewis/killedbypolice), and a Ruby script by [atom-morgan](https://github.com/atom-morgan/killed-by-police). We have not tried any of these, or used them here.

There are several other major projects with similar data:

-   <https://mappingpoliceviolence.org/> contains data on people killed by police since 2013. They aggregate data from [*Fatal Encounters*](http://www.fatalencounters.org/), KilledbyPolice.net, and the U.S. Police Shootings Database. They include data about whether the deceased was armed or not, and if a vehicle was involved, how it contributed to the death. The site features numerous visualisations exploring the data.
-   [*Fatal Encounters*](http://www.fatalencounters.org/) is a database of all deaths through police interaction in the United States since 1 Jan 2000. It is a public [Google spreadsheet](https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/edit#gid=0). It contains variables not inlcuded in killedbypolice, such as street addresses and history of mental illness. Data are tweeted at [@fatalencounters](https://twitter.com/fatalencounters) and [@the\_decedents](https://twitter.com/the_decedents). They are still updating their data. It has been used by [Snopes.com](https://www.snopes.com/do-police-kill-more-whites-than-black-people/).
-   *The Washington Post* started compiling data on fatal shootings by U.S. police in 2015. Their data are available in CSV files at their [Washington Post GitHub repository](https://github.com/washingtonpost/data-police-shootings). They are still updating their data, and it includes some variables not collected by the killedbypolice, such as whether or not the deceased was armed, and what city the event occured in. These data have also been used by [Buzzfeed](https://github.com/BuzzFeedNews/2015-12-fatal-police-shootings).
-   *The Guardian* collected data on on fatal shootings by U.S. police in 2015-2016 for their project [The Counted](https://www.theguardian.com/us-news/ng-interactive/2015/jun/01/the-counted-police-killings-us-database). They are no longer updating this project. These data have been used by [FiveThirtyEight](https://github.com/fivethirtyeight/data/tree/master/police-killings) and [Buzzfeed](https://github.com/BuzzFeedNews/2015-12-fatal-police-shootings) and projects on GitHub, e.g. [flother](https://github.com/flother/thecounted), and [Kaggle](https://www.kaggle.com/the-guardian/the-counted).
-   [VICE News](https://news.vice.com/en_us/article/xwvv3a/shot-by-cops) examined both fatal and nonfatal incidents from 2010 through 2016. The data are on [GitHub](https://github.com/vicenews/shot-by-cops/) and in a [Google Sheet](https://docs.google.com/spreadsheets/d/1CaOQ7FUYsGFCHEqGzA2hlfj69sx3GE9GoJ40OcqI9KY/edit#gid=1271324584)
-   [Wikipedia](https://en.wikipedia.org/wiki/List_of_killings_by_law_enforcement_officers_in_the_United_States) has lists of killings by law enforcement offiers in the US, but the numbers are much lower than any of the other sources noted here.
