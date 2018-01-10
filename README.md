
<!-- README.md is generated from README.Rmd. Please edit that file -->
killedbypolice
==============

<!--
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--01--09-brightgreen.svg)](https://github.com/benmarwick/killedbypolice/commits/master)  
[![Travis build status](https://travis-ci.org/benmarwick/killedbypolice.svg?branch=master)](https://travis-ci.org/benmarwick/killedbypolice)   [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
-->
The goal of killedbypolice is to make readily available the data collected by <http://killedbypolice.net/> for exploration, visualisation, and analysis.

We don't know much about who collects the data for <http://killedbypolice.net/>, or what their methods are. [FiveThirtyEight](https://fivethirtyeight.com/features/another-much-higher-count-of-police-homicides/) reported that he was 'an instructor on nonviolent physical-intervention techniques and that he prefers to remain anonymous'.

This is an important data set because the 'US government has no comprehensive record of the number of people killed by law enforcement.' ([*The Guardian*, 1 June 2015](https://www.theguardian.com/us-news/ng-interactive/2015/jun/01/about-the-counted)). The killedbypolice project is one of a few non-government projects that continuously collect data on police killings (see [Related work](#related-work) below).

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
```

Here are some ways we can take a look at the structure of the data:

``` r
# inspect the data
suppressPackageStartupMessages(library(dplyr))
glimpse(kbp2013_2017)
#> Observations: 5,578
#> Variables: 13
#> $ state          <chr> "WA", "CA", "NC", "PA", "CA", "CO", "CA", "LA",...
#> $ name           <chr> "James M. Eshelman", "Dontae Hayes", "Ricky Jun...
#> $ age            <dbl> 54, 20, 34, 43, 31, 43, 45, 22, 30, 51, 37, 40,...
#> $ gender         <chr> "M", "M", "M", "M", "F", "M", "M", "M", "M", "M...
#> $ race_ethnicity <chr> NA, "B", "B", "B", "W", "W", "O", NA, "W", "W",...
#> $ method_1       <chr> "G", "G", "G", "G", "G", "G", "G", "G", "G", "G...
#> $ method_2       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
#> $ method_3       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
#> $ method_4       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
#> $ method_5       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
#> $ event_year     <dbl> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013,...
#> $ event_month    <dbl> 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,...
#> $ event_day      <int> 31, 31, 31, 30, 30, 29, 29, 29, 29, 29, 28, 28,...

head(kbp2013_2017)
#> # A tibble: 6 x 13
#>   state name     age gend~ race~ meth~ meth~ meth~ meth~ meth~ even~ even~
#>   <chr> <chr>  <dbl> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <dbl> <dbl>
#> 1 WA    James~  54.0 M     <NA>  G     <NA>  <NA>  <NA>  <NA>   2013  12.0
#> 2 CA    Donta~  20.0 M     B     G     <NA>  <NA>  <NA>  <NA>   2013  12.0
#> 3 NC    Ricky~  34.0 M     B     G     <NA>  <NA>  <NA>  <NA>   2013  12.0
#> 4 PA    Willi~  43.0 M     B     G     <NA>  <NA>  <NA>  <NA>   2013  12.0
#> 5 CA    Betha~  31.0 F     W     G     <NA>  <NA>  <NA>  <NA>   2013  12.0
#> 6 CO    Chris~  43.0 M     W     G     <NA>  <NA>  <NA>  <NA>   2013  12.0
#> # ... with 1 more variable: event_day <int>
```

Some explorations
-----------------

Here are some explorations of the data:

The most common age to be killed by police is in the late twenties and early thirties, and this has not changed much over time.

``` r
library(ggplot2)
library(ggridges)

kbp2013_2017 %>% 
  filter(gender %in% c("F", "M", "T")) %>% 
  filter(!is.na(event_year)) %>% 
  ggplot(aes(x = age,
             y = factor(event_year),
             fill = gender)) +
  geom_density_ridges(alpha = 0.5, 
                      scale = 0.9)  +
  theme_ridges(font_size = 10) +
  scale_x_continuous(breaks = seq(0, 100, 10),
                     labels = seq(0, 100, 10)) +
  xlab("Age at death (years)") +
  ylab("Year") +
  theme(axis.title = element_text(size = 14))
```

![](readme-figs/unnamed-chunk-2-1.png)

Of the three ethnic groups that make up most of the deaths, Black and Latino people tend to be younger than White people when they are killed by police.

``` r
kbp2013_2017 %>% 
  filter(race_ethnicity %in% c("B", "W", "L")) %>% 
  filter(!is.na(event_year)) %>% 
  ggplot(aes(x = age,
             y = factor(event_year),
             fill = race_ethnicity)) +
  geom_density_ridges(alpha = 0.6, 
                      scale = 0.9)  +
  theme_ridges(font_size = 10) +
  scale_x_continuous(breaks = seq(0, 100, 10),
                     labels = seq(0, 100, 10)) +
  xlab("Age at death (years)") +
  ylab("Year") +
  theme(axis.title = element_text(size = 14))
```

![](readme-figs/unnamed-chunk-3-1.png)

By far the most common way that people are killed by police is with a gun. Deaths by vehicle involve women more often than men.

``` r
kbp2013_2017 %>% 
  filter(!is.na(event_year)) %>% 
  filter(method_1 != "NA") %>% 
  filter(gender %in% c("M", "F", "T")) %>% 
  group_by(event_year, 
           gender,
           method_1) %>% 
  tally() %>% 
  mutate(perc = n / sum(n) * 100)  %>% 
  ggplot(aes(method_1,
             perc,
             fill = gender)) +
  geom_col() +
  facet_grid(gender~event_year) +
  theme_minimal(base_size = 14) +
  xlab("Method of killing") +
  ylab("Percentage of all\npeople killed by police\nby gender") 
```

![](readme-figs/unnamed-chunk-4-1.png)

In 2016, the state with the largest number of people killed by police was California.

``` r
library(statebins) # using GitHub version
library(viridis)

# we need to convert state abbreviations to state names for the statebins function
state_abb <- data_frame(state_name = state.name,
                        state_abb = state.abb)

# we need to add the state popluations so we can get a proportion of people in each state
# we got this from https://www2.census.gov/programs-surveys/popest/tables/2010-2016/state/totals/nst-est2016-01.xlsx
state_populations <- readr::read_csv("data-raw/nst-est2016-01.csv")

# clean it a little
state_populations <-  
  state_populations %>% 
  mutate(state_name = gsub("\\.", "", X__1)) %>%
  left_join(state_abb)

# compute deaths by state and as deaths per 1000 people in each state
by_state <- kbp2013_2017 %>% 
  filter(event_year == 2016) %>% 
  group_by(state) %>% 
  tally() %>% 
  left_join(state_abb, by = c('state' = 'state_abb')) %>% 
  filter(!is.na(state_name)) %>% 
  left_join(state_populations) %>% 
  mutate(per_n_people = (n / `2016`) * 1000000)

# plot 'statebin' style map
ggplot(by_state, 
       aes(state = state_name, 
           fill = n)) +
  geom_statebins() +
  coord_equal() +
  scale_fill_viridis() +
  theme_statebins() +
  labs(title = "Total number of people killed by police in each state in 2016") +
  theme(legend.title=element_blank()) 
```

![](readme-figs/unnamed-chunk-5-1.png)

But when we consider the differences in the number of people in each state, New Mexico and Alaska have the highest proportions of people killed by police.

``` r
ggplot(by_state, 
       aes(state = state_name, 
           fill = per_n_people)) +
  geom_statebins() +
  coord_equal() +
  scale_fill_viridis() +
  theme_statebins() +
  labs(title = "Number of people killed by police in each state in 2016,\nper 1,000,000 people")  +
  theme(legend.title=element_blank()) 
```

![](readme-figs/unnamed-chunk-6-1.png)

There does not appear to any obvious seasonality to police killings. June 2013 and July 2015 were especially bad months.

``` r
kbp2013_2017 %>% 
  filter(!is.na(event_month)) %>% 
  group_by(event_month, 
           event_year) %>% 
  tally() %>% 
  mutate(month_name = month.abb[event_month]) %>% 
  mutate(month_name = factor(month_name, levels = month.abb)) %>% 
ggplot() +
  geom_tile(aes(x = month_name, 
                y = event_year, 
                fill = n)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  xlab("Month") +
  ylab("Year")
```

![](readme-figs/unnamed-chunk-7-1.png)

Have any states seen strong upward or downward trends over time? It's difficult to see if we plot them all, so let's search for states with strong trends.

``` r
n_over_time_by_state <- 
kbp2013_2017 %>% 
  group_by(state,
           event_year) %>% 
  tally() %>% 
  ungroup()

ggplot(n_over_time_by_state,
       aes(event_year,
           n,
           colour = state)) +
  geom_line() +
  geom_text(data = n_over_time_by_state[n_over_time_by_state$n > 50 & n_over_time_by_state$event_year == 2017,],
           aes(label = state,
               x = 2017.1, 
               y = n, 
               colour = state, 
               hjust = -.01)) +
  theme_minimal()  +
  ylab("Number of people killed by police") +
  xlab("Year") +
  theme(legend.position="none")
```

![](readme-figs/unnamed-chunk-8-1.png)

Let's compute a linear model for the number of killings and years for each state, then see which states have the most extreme trends. We can used a nested data frame to do this efficiently (based on the example in Grolemund and Wickham's book [*R for Data Science*](http://r4ds.had.co.nz/many-models.html)):

``` r
library(tidyr)
nested_df <- 
n_over_time_by_state %>% 
  filter(state != "") %>% 
  filter(!is.na(event_year)) %>% 
  nest(-state)
```

Which looks like this:

``` r
head(nested_df)
#> # A tibble: 6 x 2
#>   state data            
#>   <chr> <list>          
#> 1 AK    <tibble [5 x 2]>
#> 2 AL    <tibble [5 x 2]>
#> 3 AR    <tibble [5 x 2]>
#> 4 AZ    <tibble [5 x 2]>
#> 5 CA    <tibble [5 x 2]>
#> 6 CO    <tibble [5 x 2]>
```

And the *tibble* that we see in each row looks something like this:

``` r
nested_df$data[1]
#> [[1]]
#> # A tibble: 5 x 2
#>   event_year     n
#>        <dbl> <int>
#> 1       2013     2
#> 2       2014     2
#> 3       2015     6
#> 4       2016     8
#> 5       2017     8
```

Now let's compute linear models for all states:

``` r
library(purrr)
model_by_state <- 
nested_df %>% 
  mutate(model = map(data, ~lm(event_year ~ n, data = .)))
```

Let's only look at the states where the linear model has a p-value of less than 0.05 and the adjusted R<sup>2</sup> is &gt;0.7. This will limit us to states that have a statistically significant trend over time:

``` r
glance <- model_by_state %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)

glance_sig <- 
  glance %>% 
  filter(adj.r.squared >= 0.7) %>% 
  filter(p.value <= 0.05) %>% 
  arrange(desc(adj.r.squared))

glance_sig
#> # A tibble: 7 x 12
#>   state r.squ~ adj.r.~ sigma stati~ p.value    df logLik   AIC   BIC devi~
#>   <chr>  <dbl>   <dbl> <dbl>  <dbl>   <dbl> <int>  <dbl> <dbl> <dbl> <dbl>
#> 1 AL     0.943   0.923 0.438   49.2 0.00595     2  -1.69  9.37  8.20 0.574
#> 2 CO     0.931   0.908 0.479   40.5 0.00785     2  -2.14 10.3   9.11 0.690
#> 3 WV     0.908   0.878 0.552   29.8 0.0121      2  -2.85 11.7  10.5  0.916
#> 4 AK     0.880   0.841 0.631   22.1 0.0182      2  -3.52 13.0  11.9  1.20 
#> 5 NM     0.849   0.799 0.709   16.9 0.0261      2  -4.10 14.2  13.0  1.51 
#> 6 WI     0.816   0.755 0.782   13.3 0.0355      2  -4.59 15.2  14.0  1.84 
#> 7 VA     0.785   0.713 0.847   10.9 0.0455      2  -4.99 16.0  14.8  2.15 
#> # ... with 1 more variable: df.residual <int>
```

And when we plot only these states with the signficant trends we see that they all indicate a trend of *increasing* deaths by police over time.

No state seems to have succeeded in reducing the total number of people killed by police over time.

``` r
n_over_time_by_state_sig <- 
n_over_time_by_state %>% 
  filter(state %in% glance_sig$state) 

ggplot(n_over_time_by_state_sig,
       aes(event_year,
           n,
           group = state,
           colour = state)) +
  geom_line() +
  geom_text(data = n_over_time_by_state_sig[ n_over_time_by_state_sig$event_year == 2017,],
           aes(label = state,
               x = 2017.1, 
               y = n, 
               colour = state, 
               hjust = -.01)) +
  theme_minimal() +
  ylab("Number of people killed by police") +
  xlab("Year")
```

![](readme-figs/unnamed-chunk-14-1.png)

Related work
------------

There are several Python scripts for scraping killedbypolice.net, for example by [markberger](https://github.com/markberger/police-killings-dataset) and [AceLewis](https://github.com/AceLewis/killedbypolice), and a Ruby script by [atom-morgan](https://github.com/atom-morgan/killed-by-police). We have not tried any of these, or used them here.

There are several other major projects with similar data:

-   [Mapping Police Violence](https://mappingpoliceviolence.org/) contains data on people killed by police since 2013. They aggregate data from [*Fatal Encounters*](http://www.fatalencounters.org/), killedbypolice.net, and the [U.S. Police Shootings Database](https://docs.google.com/spreadsheets/d/1cEGQ3eAFKpFBVq1k2mZIy5mBPxC6nBTJHzuSWtZQSVw/edit). They include data about whether the deceased was armed or not, and if a vehicle was involved, how it contributed to the death. The site features numerous visualisations exploring the data, especially the representation of Black people, and includes some interactive visualisations.
-   [*Fatal Encounters*](http://www.fatalencounters.org/) is a database of all deaths through police interaction in the United States since 1 Jan 2000. It is a public [Google spreadsheet](https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/edit#gid=0). It contains variables not inlcuded in killedbypolice, such as street addresses and history of mental illness. Data are tweeted at [@fatalencounters](https://twitter.com/fatalencounters) and [@the\_decedents](https://twitter.com/the_decedents). They are still updating their data. It has been used by [Snopes.com](https://www.snopes.com/do-police-kill-more-whites-than-black-people/).
-   *The Washington Post* started compiling data on fatal shootings by U.S. police in 2015. Their data are available in CSV files at their [Washington Post GitHub repository](https://github.com/washingtonpost/data-police-shootings). They are still updating their data, and it includes some variables not collected by the killedbypolice, such as whether or not the deceased was armed, and what city the event occured in. These data have also been used by [Buzzfeed](https://github.com/BuzzFeedNews/2015-12-fatal-police-shootings).
-   *The Guardian* collected data on on fatal shootings by U.S. police in 2015-2016 for their project [The Counted](https://www.theguardian.com/us-news/ng-interactive/2015/jun/01/the-counted-police-killings-us-database). They are no longer updating this project. These data have been used by [FiveThirtyEight](https://github.com/fivethirtyeight/data/tree/master/police-killings) and [Buzzfeed](https://github.com/BuzzFeedNews/2015-12-fatal-police-shootings) and projects on GitHub, e.g. [flother](https://github.com/flother/thecounted), and [Kaggle](https://www.kaggle.com/the-guardian/the-counted).
-   [VICE News](https://news.vice.com/en_us/article/xwvv3a/shot-by-cops) examined both fatal and nonfatal incidents from 2010 through 2016. The data are on [GitHub](https://github.com/vicenews/shot-by-cops/) and in a [Google Sheet](https://docs.google.com/spreadsheets/d/1CaOQ7FUYsGFCHEqGzA2hlfj69sx3GE9GoJ40OcqI9KY/edit#gid=1271324584)
-   [Wikipedia](https://en.wikipedia.org/wiki/List_of_killings_by_law_enforcement_officers_in_the_United_States) has lists of killings by law enforcement offiers in the US, but the numbers are much lower than any of the other sources noted here.

Contributing
------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
