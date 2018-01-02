#' Import the data from the webpages into R objects
#'
#'

#' Here are the web pages that we downloaded on 1 Jan 2018, using Chrome Version 63.0.3239.84 (Official Build) (64-bit)
webpages <- list.files(pattern = ".html$", full.names = TRUE)

# read in the webapges into the R session
library(tidyverse)
all_pages <- map(webpages, ~read_table(.x))

#' filter to keep only columns that have data in them, get rid of some HTML
x2 <- map(all_pages, ~.x %>%
            filter(grepl("<tr><td><center>", X1)))

library(glue)
x3 <- map(x2, ~.x %>%
            # split variables into cols
            separate(X1,
                     into = glue('V{1:10}'),
                     sep = "<td>",
                     remove = FALSE))


x4 <- map(x3 , ~.x %>%
            separate(V2,
                     into = c('date', 'date2',
                              'date3', 'date4'),
                     sep = "<br>"))

#' format dates, get rid of white space
x5 <- map(x4, ~.x %>%
            mutate_all(funs(trimws)) %>%
            mutate(date = str_replace_all(date,
                                          "<center>\\([0-9]*\\)|</center></td>|[:punct:]",
                                          "")) %>%
            mutate(date2 = str_replace_all(date2,
                                           "\\([0-9]*\\)|</center></td>|[:punct:]",
                                           "")) %>%
            mutate_all(funs(trimws)) %>%
            mutate(date_format =  as.Date(date, "%B %d %Y")) %>%
            mutate(date_format2 =  as.Date(date2, "%B %d %Y"))
            )

#'  remove URLs in name col, split mulitple desceased into multiple cols
x6 <- map(x5, ~.x %>%
            mutate(deceased = gsub('http\\S+\\s*', "", V5)) %>%
            mutate(deceased = gsub('<a href="|</a>|target="new"|</td>', "", deceased)) %>%
            # make separate row where there are two deceased in one row
            separate_rows(deceased, sep = "<br>") %>%
            mutate(deceased = str_replace_all(deceased, "<|>", "")) %>%
            # get the name and age in their own cols
            separate(deceased,
                     into = c('deceased_name', 'deceased_age'),
                     sep = ",(?=[^,]+$)",  # split on the last comma
                     remove = FALSE)  %>%
            mutate(deceased_age = as.numeric(trimws(deceased_age))))

# split gender and race col into separate cols
x7 <-  map(x6, ~.x %>%
            mutate(V4 = str_replace_all(V4, "</td>", "")) %>%
            separate(V4,
                     into = c('gender_race1',
                              'gender_race2',
                              'gender_race3'),
                     sep = "<br>") %>%
            separate(gender_race1,
                     into = c('gender1',
                              'race1'),
                     sep = "/") %>%
            separate(gender_race2,
                     into = c('gender2',
                              'race2'),
                     sep = "/"))


# separate out multiple methods
x8 <- map(x7, ~.x %>%
            mutate(V6 = str_replace_all(V6, '<font size="2">|</font></td>', "")) %>%
            mutate(V6 = trimws(V6)) %>%
            separate(V6,
                     into = glue('method_{1:10}'),
                     sep = "<br>"))

# tidy state column
x9 <- map(x8, ~.x %>%
            mutate(State = str_replace_all(V3, "</td>", "")) %>%
            mutate(State = trimws(State)) %>%
            select(-V3))



#------------------------------------------------------------------------------

#' convert list of data frames to one big data frame
x10 <- bind_rows(x9)

#' We have `r nrow(x9)` rows.
#'
#' remove some less informative rows
x10 <- select(x10,
             -X1,
             -V1,
             -V5,
             -V7,
             -V8,
             -V9,
             -V10)

#------------------------------------------------------------------------------

#' What does it look like? Here's a snippet of just a few columns:
x10 %>%
  select(date, State, deceased_name, deceased_age) %>%
  head(n = 10) %>%
  knitr::kable()

#' ## Some quick plots

ggplot(x10,
       aes(deceased_age)) +
  geom_histogram() +
  theme_bw()

#' age and gender
age_and_gender <-
  x10 %>%
  select(deceased_age, gender1) %>%
  filter(!is.na(deceased_age)) %>%
  mutate(age_category=cut(deceased_age,
                          breaks = seq(0,max(deceased_age), 5))) %>%
  filter(!is.na(age_category)) %>%
  group_by(age_category) %>%
  summarise(prop_female = sum(gender1 == 'F') / n())

ggplot(age_and_gender,
       aes(age_category,
           prop_female)) +
  geom_point(size = 3) +
  ylab("Proportion of deaths that are Female") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("Female deaths by police, by age group",
          subtitle = glue("Data from kpb"))

#' race
race <-
  x6 %>%
  select(deceased_age, race) %>%
  filter(!is.na(race)) %>%
  filter(!is.na(deceased_age)) %>%
  mutate(race = str_sub(race, 1, 1)) %>%
  filter(race %in% c("I", "B", "W", "L", "A", "P", "O"))

# get n to use in tick labels
race_n <-
  race %>%
  group_by(race) %>%
  tally() %>%
  mutate(race_label = glue('{race}\n(n = {n})')) %>%
  right_join(race)

library(ggbeeswarm)
ggplot(race_n,
       aes(reorder(race_label,
                   -deceased_age),
           deceased_age)) +
  geom_quasirandom(alpha = 0.1) +
  xlab("Race (as classified by killedbypolice.net)") +
  theme_bw()  +
  ggtitle("Deaths by police, by age and race of deceased",
          subtitle = glue("Data from {url}"))

#' method of death
method_of_death <-
  x6 %>%
  mutate(method_long = case_when(
    method == "G" ~ "Gun",
    method == "T" ~ "Taser",
    method == "R" ~ "Restraint/\nPhysical Force",
    method == "C" ~ "Chemical",
    method == "V" ~ "Vehicle",
    method == "O" ~ "Other"))

method_of_death_df <-
  method_of_death %>%
  group_by(method_long) %>%
  tally() %>%
  mutate(prop = n / sum(n) ) %>%
  filter(!is.na(method_long))


ggplot(method_of_death_df,
       aes(reorder(method_long, -prop),
           prop)) +
  geom_col() +
  xlab("") +
  ylab("Proportion of all deaths by police") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#' Let's convert n to the proportion of people in each state

#' age distribution by state
#' https://hafen.github.io/geofacet/
library("geofacet")

ggplot(x6,
       aes(deceased_age)) +
  geom_histogram() +
  theme_bw() +
  facet_geo(~ V3)

#'  hexbin state map
#'  based on https://rud.is/b/2015/05/15/u-s-drought-monitoring-with-hexbin-state-maps-in-r/
x7 <-
  x6 %>%
  group_by(V3) %>%
  tally() %>%
  left_join(state_pops, by = c("V3" = "state_abb")) %>%
  mutate(prop =  n / Census * 100000) # per 100k people

library(rgdal)
library(rgeos)

# get map from
download.file("https://gist.githubusercontent.com/hrbrmstr/51f961198f65509ad863/raw/219173f69979f663aa9192fbe3e115ebd357ca9f/us_states_hexgrid.geojson", "us_states_hexgrid.geojson")
us <- readOGR("us_states_hexgrid.geojson", "OGRGeoJSON")
centers <- cbind.data.frame(data.frame(gCentroid(us, byid=TRUE), id=us@data$iso3166_2))
us_map <- fortify(us, region="iso3166_2")

gg <- ggplot()

gg <- gg + geom_map(
  data = us_map,
  map = us_map,
  aes(x = long,
      y = lat,
      map_id = id),
  color = "white",
  size = 0.5
)

gg <- gg + geom_map(data = x7,
                    map = us_map,
                    aes(fill = prop,
                        map_id = V3))

gg <- gg + geom_map(
  data = x7,
  map = us_map,
  aes(map_id = V3),
  fill = "#ffffff",
  alpha = 0,
  color = "white",
  show.legend = FALSE
)

gg <- gg + geom_text(data=centers, aes(label=id, x=x, y=y), color="white", size=4)

gg <- gg + coord_map() +
  scale_fill_viridis_c() +
  theme_bw()  +
  xlab("") +
  ylab("")

gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(panel.spacing=unit(3, "lines"))
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())
gg <- gg + theme(strip.background=element_blank())
gg <- gg + theme(strip.text=element_text(face="bold", hjust=0, size=14))
gg <- gg + ggtitle(glue('Killed by police, per 100k people, {min(years)}-2017'),
                   subtitle = "Data from http://killedbypolice.net/")

gg
#' This seems right, cf. https://mappingpoliceviolence.org/states/
#'
#' interactive map of raw counts by state over time
#'
state_pops_long <-
  state_pops %>%
  select(state_abb, one_of(as.character(years))) %>%
  gather(year, population, -state_abb) %>%
  filter(!is.na(state_abb))


x8 <-
  x6 %>%
  mutate(month = format(date_format, "%m"),
         year = format(date_format, "%Y"),
         month_year = glue('{month}/{year}'),
         state_abb = V3) %>%
  group_by(month, year, month_year, state_abb) %>%
  tally() %>%
  arrange(year, month) %>%
  left_join(state_pops_long)

#'  get an ordered factor to make the x-axis plot in order
x8$month_year <- factor(x8$month_year, levels = unique(x8$month_year))

gg1 <-
  ggplot(x8,
         aes(month_year,
             n)) +
  geom_line(aes(group = state_abb,
                colour = state_abb)) +
  scale_color_viridis_d() +
  # scale_y_log10() +
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  ggtitle(glue('Raw counts of people killed by police, {min(years)}-2017'),
          subtitle = "Data from http://killedbypolice.net/")

gg1

# get interactive plot
plotly::ggplotly(gg1)


#' ## No need to read below here
#' This is just drafts of the code that I used above

#------------------------------------------------------------------------------
# No need to look here, it's just what I did to get the 2017 data only
# for one webpage, the KBP 2017 data
# get HTML page as raw HTML
tmp1 <- readr::read_table(url)
# rename the column for ease of use
names(tmp1) <- "V1"


# filter to keep only columns that have data in them, get rid of some HTML
tmp2 <- tmp1 %>%
  mutate(V99 = gsub('<center>|<CENTER>|<font size=2>|target=new', "", V1)) %>%
  filter(grepl("<TR><TD>\\(", V99)) %>%
  select(-V1)


tmp3 <- tmp2 %>%
  # split variables into cols
  separate(V99,
           into = glue('V{1:9}'),
           sep = "<td>|<TD>",
           remove = FALSE) %>% # keep it so we can check on things later
  separate(V2,
           into = c('number', 'date',
                    'number2', 'date2'),
           sep = "\\)|<br>|\\)|<BR>")

# format some col classes, numeric, date, get rid of white space
tmp4 <-
  tmp3 %>%
  mutate_all(funs(trimws)) %>%
  mutate(number = as.numeric(gsub("\\(", "", number))) %>%
  mutate(date_format =  as.Date(date, "%B %d, %Y"))

# remove URLs in name col, split mulitple desceased into multiple cols
tmp5 <-
  tmp4 %>%
  mutate(deceased = gsub('http\\S+\\s*', "", V5)) %>%
  mutate(deceased = gsub('<a href=">|</a>', "", deceased)) %>%
  # make separate row where there are two deceased in one row
  separate_rows(deceased, sep = "<br>") %>%
  # get the name and age in their own cols
  separate(deceased,
           into = c('deceased_name', 'deceased_age'),
           sep = ",(?=[^,]+$)",  # split on the last comma
           remove = FALSE)  %>%
  mutate(deceased_age = as.numeric(trimws(deceased_age))) %>%
  separate(V4,
           into = c('gender', 'race'),
           sep = "/")


# How many are we missing?
max_number_on_website <- max(tmp5$number)
number_that_we_have <- nrow(tmp5)
difference <- max_number_on_website - number_that_we_have
difference


# is this from your group? https://rpubs.com/johnbradford/policeShootDataCreation




