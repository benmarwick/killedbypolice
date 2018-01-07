#' Import the data from the webpages into R objects
#'
#------------------------------------------------------------------------------

#'
#' Here are the web pages that we downloaded on 1 Jan 2018, using Chrome Version 63.0.3239.84 (Official Build) (64-bit)
library(here)
webpages <- list.files(here("/data-raw"), pattern = ".html$", full.names = TRUE)

# read in the webapges into the R session
library(tidyverse)
all_pages <- map(webpages, ~read_table(.x))

#------------------------------------------------------------------------------


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

#------------------------------------------------------------------------------

#' convert list of data frames to one big data frame
x4 <- bind_rows(x3)

#------------------------------------------------------------------------------



# we want to separate rows on V2, V4, V5 (date, sex/race, name), but these are sometime
# not exactly equal when there are mulitple deceased on one event. So we need to
# tidy it a bit.

# how many <br> in each col?
x5 <- x4  %>%
          mutate(br_in_date = str_count(V2, "<br>")) %>%
          mutate(br_in_genderrace = str_count(V4, "<br>")) %>%
          mutate(br_in_nameage = str_count(V5, "<br>")) %>%
          mutate(max_br = pmax(br_in_date, br_in_genderrace, br_in_nameage)) %>%
            rowwise() %>%
          mutate(which_has_max = which.max(c(br_in_date,
                                             br_in_genderrace,
                                             br_in_nameage)))

# insert some <br> into cols if they don't have the max number for that row

x5$genderrace_new <- 0
x5$nameage_new <- 0
x5$date_new <- 0

for(i in seq_len(nrow(x5))){

  x5$genderrace_new[i] <-
    with(x5, ifelse((br_in_date[i] > br_in_genderrace[i]),
           paste(V4[i], rep("<br>.", (br_in_date[i] - br_in_genderrace[i]))),
           V4[i]))

  x5$genderrace_new[i] <-
    with(x5, ifelse(br_in_nameage[i] > str_count(genderrace_new[i], "<br>"),
                    paste(genderrace_new[i], rep("<br>.", (br_in_nameage[i] - br_in_genderrace[i]))),
                    genderrace_new[i]))

  x5$nameage_new[i] <-
    with(x5, ifelse((br_in_date[i] > br_in_nameage[i]),
           paste(V5[i], rep("<br>.", (br_in_date[i] - br_in_nameage[i]))),
           V5[i]))

  x5$date_new[i] <-
    with(x5, ifelse((br_in_genderrace[i] > br_in_date[i]),
                    paste(V2[i], rep(glue("<br>.{V2[i]}"), (br_in_genderrace[i] - br_in_date[i]))),
                    V2[i]))

  x5$date_new[i] <-
    with(x5, ifelse(br_in_nameage[i] > str_count(date_new[i], "<br>"),
                    paste(date_new[i], rep(glue("<br>.{date_new[i]}"), (br_in_nameage[i] - br_in_date[i]))),
                    date_new[i]))


}

# check that we have the same number of <br> in those three cols
# x6 <- x5  %>%
#   mutate(br_in_date2 = str_count(date_new, "<br>")) %>%
#   mutate(br_in_genderrace2 = str_count(genderrace_new, "<br>")) %>%
#   mutate(br_in_nameage2 = str_count(nameage_new, "<br>")) %>%
#   mutate(max_br2 = pmax(br_in_date2, br_in_genderrace2, br_in_nameage2))
#------------------------------------------------------------------------------


#' Continue cleaning, format dates, get rid of white space, before we separate rows
x6 <- x5 %>%
  mutate_all(funs(trimws)) %>%
  mutate(date = str_replace_all(date_new,
                                "<center>\\([0-9]*\\)|</center></td>|[:punct:]",
                                "")) %>%
  mutate_all(funs(trimws)) %>%
  mutate(date_format =  as.Date(date, "%B %d %Y"))

#------------------------------------------------------------------------------


#' Now that we've made sure we have an equal number of <br> in each col,
#' we can separate rows on <br> to get multiple deceased in one event
#' in one row each
x7 <- x6 %>%
      separate_rows(c(date_new, genderrace_new, nameage_new),
                     sep = "<br>")

#------------------------------------------------------------------------------


#'  remove URLs in name col, split deceased into name and age
x8 <- x7 %>%
            mutate(deceased = gsub('http\\S+\\s*', "", nameage_new)) %>%
            mutate(deceased = gsub('<a href="|</a>|target="new"|</td>', "", deceased)) %>%
            mutate(deceased = str_replace_all(deceased, "<|>", "")) %>%
            # get the name and age in their own cols
            separate(deceased,
                     into = c('deceased_name', 'deceased_age'),
                     sep = ",(?=[^,]+$)",  # split on the last comma
                     remove = FALSE)  %>%
            mutate(deceased_age = as.numeric(trimws(deceased_age)))

#------------------------------------------------------------------------------


#' split gender and race col into separate cols
x9 <-  x8 %>%
            mutate(genderrace_new = str_replace_all(genderrace_new, "</td>", "")) %>%
            separate(genderrace_new,
                     into = c('gender',
                              'race'),
                     sep = "/")

#------------------------------------------------------------------------------


#' separate out multiple methods. We have to be careful here because we can see
#' multiple methods where there are multiple deceased on one event, and where
#' there is one deceased who was killed in multiple ways. Clean out the HTML
#' first...

x10 <- x9 %>%
            mutate(V6 = str_replace_all(V6, '<font size="2">|</font></td>|<center>|</center>|<font color="white">\\.</font>', "")) %>%
            mutate(method = trimws(V6))

#' Then if there are two methods, and the second is different from the first,
#' then we assume that it's one deceased with multiple methods, and we want
#' to keep those. We don't want to keep multiple same methods, not informative
#'
# unique(x10$method)
method_simplify <- function(x){
  t2 <- str_extract_all(x, "[A-Z]")
  t3 <- map_int(t2, ~length(unique(.x)))
  t4 <- map2(t2, t3, ~.x[1:.y])
  t5 <- map_chr(t4, ~paste(.x, collapse = "<br>"))
  return(t5)
}

x10$method <- method_simplify(x10$method)

x11 <-  x10 %>%
        separate(method,
                 into = glue('method_{1:5}'),
                 sep = "<br>") %>%
  mutate_all(funs(trimws))
# unique(x11$method_3)

#------------------------------------------------------------------------------


#' tidy state column
x12 <- x11 %>%
            mutate(State = str_replace_all(V3, "</td>", "")) %>%
            mutate(State = trimws(State))

#------------------------------------------------------------------------------


#' get only columns of interest & ensure type is ok
x13 <- x12 %>%
  select("date_format",
         "State",
         "deceased_name",
         "deceased_age",
         "gender",
         "race",
          "method_1",
          "method_2",
          "method_3",
          "method_4",
          "method_5"
          ) %>%
  mutate(deceased_age = as.numeric(deceased_age),
         date_format = as.Date(date_format))

#' Now we have the data frame for 2013-2017
#' Let's save to the package
kbp2013_2017 <- x13
devtools::use_data(kbp2013_2017)

#' and document


#------------------------------------------------------------------------------

#------------------------------------------------------------------------------

#' What does it look like? Here's a snippet of just a few columns:
x13 %>%
  select(date_format,
         State,
         deceased_name,
         deceased_age) %>%
  head(n = 10) %>%
  knitr::kable()

#' ## Some quick plots

ggplot(x13,
       aes(deceased_age)) +
  geom_histogram() +
  theme_bw()

#' age and gender
age_and_gender <-
  x13 %>%
  select(deceased_age, gender) %>%
  filter(!is.na(deceased_age)) %>%
  mutate(age_category=cut(deceased_age,
                          breaks = seq(0,max(deceased_age), 5))) %>%
  filter(!is.na(age_category)) %>%
  group_by(age_category) %>%
  summarise(prop_female = sum(gender == 'F') / n())

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
  x13 %>%
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
          subtitle = glue("Data from kpb"))

#' method of death
method_of_death <-
  x13 %>%
  mutate(method_long = case_when(
    method_1 == "G" ~ "Gun",
    method_1 == "T" ~ "Taser",
    method_1 == "R" ~ "Restraint/\nPhysical Force",
    method_1 == "C" ~ "Chemical",
    method_1 == "V" ~ "Vehicle",
    method_1 == "O" ~ "Other"))

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
  ylab("Proportion of all\ndeaths by police") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#' Let's convert n to the proportion of people in each state

#' age distribution by state
#' https://hafen.github.io/geofacet/
library("geofacet")

ggplot(x13,
       aes(deceased_age)) +
  geom_histogram() +
  theme_bw() +
  facet_geo(~ State)

#'  hexbin state map
#'  based on https://rud.is/b/2015/05/15/u-s-drought-monitoring-with-hexbin-state-maps-in-r/
#'  get state population data
tmp <- rio::import("https://www2.census.gov/programs-surveys/popest/tables/2010-2016/state/totals/nst-est2016-01.xlsx",
                   skip = 3)

# save it locally
write.csv(tmp, paste0(here::here("data-raw"),"/nst-est2016-01.csv"))

# clean up state names to abbreviations

state_abb <- data_frame(state_name = state.name,
                        state_abb = state.abb)
state_abb <- rbind(state_abb,
                   data_frame(
                     state_name = "District of Columbia",
                     state_abb = "DC"))
state_pops <-
  tmp %>%
  mutate(state_name = gsub("\\.", "", X__1)) %>%
  left_join(state_abb)

x14 <-
  x13 %>%
  group_by(State) %>%
  tally() %>%
  left_join(state_pops, by = c("State" = "state_abb")) %>%
  mutate(prop =  n / Census * 100000) # per 100k people

library(rgdal)
library(rgeos)

# get map from
download.file("https://gist.githubusercontent.com/hrbrmstr/51f961198f65509ad863/raw/219173f69979f663aa9192fbe3e115ebd357ca9f/us_states_hexgrid.geojson", destfile=paste0(here::here("data-raw"),"/us_states_hexgrid.geojson"))
us <- readOGR(paste0(here::here("data-raw"),"/us_states_hexgrid.geojson"), "OGRGeoJSON")
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




