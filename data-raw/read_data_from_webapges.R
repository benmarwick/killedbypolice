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

# rename a few variables, tidy date
library(lubridate)
x14 <- x13 %>%
  rename(event_date = date_format,
         state = State,
         name = deceased_name,
         age = deceased_age,
         race_ethnicity = race) %>%
  mutate(event_year = year(event_date),
         event_month = month(event_date),
         event_day = day(event_date)) %>%
  select(-event_date)

# any stray HTML still in there?
# look for it
x14 %>%
   filter_all(any_vars(grepl("<|>", .))) %>%
   View

# clear it out
x15 <- x14 %>%
  mutate_all(funs(ifelse(grepl("<|>", .), "", .)))

x15$gender <- str_replace_all(x15$gender, "\\.", "")

#' Now we have the data frame for 2013-2017
#' Let's save to the package
kbp2013_2017 <- x15
devtools::use_data(kbp2013_2017, overwrite = TRUE)

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

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
library(tidyverse)
library(glue)

years <- 1791:2018 # 228 years (or URLs to scrape)
odmp_urls <-
  glue('https://www.odmp.org/search/year?year={years}')

variables <- c("empty", "Officer", "Location", "EOW", "Cause")

library(rvest)
odmp <-
# scrape each page for each year
map(odmp_urls,
    ~read_html(.x) %>%
      html_nodes(".officer-short-details") %>%
      html_text() ) %>%
  # convert to character vector
  unlist() %>%
  # convert to data frame
  enframe() %>%
  # split 'value' column by line breaks into new cols
  separate(value, into = variables, sep = "\n") %>%
  # remove unnecessary text
  mutate(EOW = str_replace_all(EOW, "EOW:", ""),
         Cause = str_replace_all(Cause, "Cause:", ""))

# separate a few variables out

remove_me <-  ",|Police Department|Department of Natural Resources|Fish|Game|Sheriff's Office|Office of Public Safety|Constable's Office|Department of Corrections|County Sheriff's Department|Sheriff's Police Department|Department of Correctional Services|Department of Corrections|Department of Police Services|Transportation Authority Police|Division of Police|Department of Wildlife and Fisheries|Highway Patrol|County Sheriff's Office|Parish Sheriff's Office|Parish Sheriff's Department|Department of Public Safety|School District Police Services|Department of Correction|Division of School Safety|State Police|Texas Highway Patrol|Texas Highway Patrol,|Department of Criminal Justice|Police Bureau|Metropolitan Police Department| Metropolitan Police Department, MO"

library(lubridate)
odmp_tidy <-
  odmp %>%
  mutate(State = str_extract(Location, "[A-Z]{2}$"),
         City = str_replace(Location, "[A-Z]{2}$", ""),
         City = str_replace(City, remove_me, ""),
         City = str_replace(City, " \\,|[[:punct:]]", ""),
         posix_date = as.Date(str_trim(EOW), format= "%A, %B %e, %Y"),
         year = year(posix_date),
         month = month(posix_date),
         day = day(posix_date)) %>%
  select(-empty) %>%
  mutate_all(funs(str_trim))

# get the rank of the officer

ranks <- c("officer|police|patrolman|deputy|sheriff|sergeant|detective|agent|policeman|constable|special|lieutenant|patrol|k9|corporal|captain|correctional|u.s.|inspector|private|town|warden|investigator|night|guard|corrections|assistant|reserve|senior|undersheriff|watchman|ranger|conservation|railroadkeeper|boatman|marshal|keeper|city marshal|jailer|posseman|collector|chief of|turnkey|deputized civilian|collector|roundsman|roundsman|principal|third|fourth|posse member|provost|custom house|high|paramedic|elect|acting chief|juvenile detention|juvenile security|man|trooper|federal|auxiliary|first|motorcycle|correction|traffic|prison|narcotics|probation|parole|prohibition|detention|superintendent|county|probationary|security|mounted|immigration|acting|supervisor|staff|operative|merchant|commissioner|criminal|dispatcher|dispensary|colonel|supervisory|technical|narcotic|game|park|class|employee|pilot|major|village|coast|director|district|field|public|revenue|motor|cadet")

odmp_tidy$Rank <-
  odmp_tidy$Officer%>%
      str_trim() %>%
      tolower() %>%
      str_split( boundary("word")) %>%
      map(., ~str_extract(.x, ranks)) %>%
      map(., ~paste0(.x, collapse = " ") %>%
            str_replace_all("NA", "") %>%
            str_trim()) %>%
    unlist()

# save
write_csv(odmp_tidy, "odmp_data_1791_2018.csv")
# read in
odmp_tidy <- read_csv("odmp_data_1791_2018.csv")

#---------------------------------------------
# get bio details

# get URLs to each bio
odmp_page_urls <-
	# scrape each page for each year, get URL to more details
	map(odmp_urls,
			~read_html(.x) %>%
				html_nodes(".officer-short-details a") %>%
				html_attr('href')) %>%
	# convert to character vector
	unlist()

saveRDS(odmp_page_urls, "odmp_page_urls.rds")

# get web pages from the bio page of each officer
# this is pretty slow...
odmp_page_bio_page  <-
	# scrape each page for each year, get URL to more details
	map(odmp_page_urls,
			~read_html(.x))

saveRDS(odmp_page_bio_page, "odmp_page_bio_page.rds")
# now from these pages, we can extract some info:

# 1. get the bio details
odmp_page_bio_page_bio <-
			map(odmp_page_bio_page,
				~html_nodes(.x, ".officer-bio") %>%
				html_text()  )

# 2. get the names
odmp_page_bio_page_name <-
	map(odmp_page_bio_page,
	~html_nodes(.x, ".officer-incident-description h3") %>%
	html_text() ) %>%
	unlist()

# 2. get the EOW (date), location and name with rank
odmp_page_bio_page_eow <-
	map(odmp_page_bio_page,
			~html_nodes(.x, ".officer-eow") %>%
				html_text() ) %>%
	unlist()

odmp_page_bio_page_agency <-
	map(odmp_page_bio_page,
			~html_nodes(.x, ".officer-agency") %>%
				html_text() ) %>%
	unlist()

odmp_page_bio_page_name_rank <-
	map(odmp_page_bio_page,
			~html_nodes(.x, ".officer-short-details strong ") %>%
				html_text() ) %>%
	unlist()



# tidy this into cols

# save to work on later
# saveRDS(odmp_page_bio_page, "odmp_page_bio_page.rds")
odmp_page_bio_page <- readRDS("odmp_page_bio_page.rds")

# convert to data frame for each officer
variables <- c("other5", "Age" , "Tour" , "Badge", "Veteran", "Incident details", "Cause", "Incident Date", "Weapon", "Offender", "Location", "other1", "other2", "other3", "other4")

odmp_page_bio_df <-
	odmp_page_bio_page_bio %>%
	# convert to character vector
	unlist() %>%
	# convert to data frame
	enframe() %>%
	# split 'value' column by line breaks into new cols
	separate(value, into = variables, sep = "\n")


# add the names, EOW, location, remove the dogs...
library(lubridate)
odmp_page_bio_df_names_eow_date <-
	odmp_page_bio_df %>%
	mutate(name = odmp_page_bio_page_name) %>%
	mutate(name_and_rank = odmp_page_bio_page_name_rank) %>%
	mutate(eow = str_replace(odmp_page_bio_page_eow, "End of Watch ", "")) %>%
	mutate(posix_date = as.Date(str_trim(eow), format= "%A, %B %e, %Y"),
		     year = year(posix_date),
         month = month(posix_date),
         day = day(posix_date))  %>%
	filter(!str_detect(Age, "Breed"))

# The bio pages do not all have the same structure, so some of the
# fields are staggered across the wrong columns. We need to scan all
# columns for the values for each variable of interest. Fortunately,
# most of the interesting values have a prefix we can use to find them

# regex for each variable:
# Age
# Tour
# Badge
# Incident Details
# Cause
# Weapon
# Offender
# Location

# Find these wherever they are!
find_in_any_cell <-
	function(dfr, var){
		var <- tolower(var)
		# create dataframe to store output
		out <- as_data_frame(matrix(NA,
																nrow = nrow(dfr),
														    ncol = ncol(dfr)))

		# search each column for rows that match the input
		for(i in 1:ncol(dfr)){
			dfr_this_col <- str_trim(tolower(pull(dfr[,i])))
			out[,i] <- ifelse(str_detect(dfr_this_col,
																	 glue('{var}.*')),
												dfr_this_col, NA)
		}
		# drop cols that are all NA
		out <- Filter(function(x)!all(is.na(x)), out)
		return(out)

	}


# keywords we want to find in the df, this will make a list of
# one df for each keyword
keywords <- c("Age",
							"Tour",
							"Badge",
							"Incident Details",
							"Cause",
							"Weapon",
							"Offender",
							"lat")
dfs_with_keywords <- map(
	keywords,
	~ find_in_any_cell(odmp_page_bio_df_names_eow_date, .x) %>%
		unite(col = "x") %>%
		mutate(x = str_replace_all(x, "_|NA", ""))
)
# bind them into a dataframe
keywords_df <- bind_cols(dfs_with_keywords)
names(keywords_df) <- keywords

# remove redundant text in the data
keywords_df_clean <-
keywords_df %>%
	mutate_all(funs(str_replace_all(.,
																	collapse(c(tolower(keywords),
																						 "not available"),
																					 "|"),
																	""))) %>%
	mutate_all(str_trim) %>%
  mutate(coords = str_replace_all(lat, '[[:alpha:]]|\\{|\\}|\\:|\\"', "")) %>%
	separate(coords, into = c("lat", "long"), ",") %>%
	mutate_at(vars(lat, long), as.numeric) %>%
	# get the names, dates, etc.  back on there
	mutate(name = odmp_page_bio_df_names_eow_date$name,
				 name_and_rank = odmp_page_bio_df_names_eow_date$name_and_rank,
				 eow = odmp_page_bio_df_names_eow_date$eow,
				 posix_date = odmp_page_bio_df_names_eow_date$posix_date,
				 year = odmp_page_bio_df_names_eow_date$year,
				 month = odmp_page_bio_df_names_eow_date$month,
				 day = odmp_page_bio_df_names_eow_date$day) %>%
	separate(name, into = c('first_name',
											'other_name1',
											'other_name2',
											'other_name3'),
			 sep = " ",
			 remove = FALSE) %>%
	# years are limited for this fn, so
	mutate(year_rounded = ifelse(year < 1880, 1880,
											 ifelse(year > 2012, 2012,
											 			 year)))


# get the gender of each officer, this isn't provided by the site,
# so we'll infer it from social security data
library(gender)

keywords_df_clean_gender <-
	keywords_df_clean %>%
	select(first_name,
				 year)  %>%
	# years are limited for this fn, so
	mutate(year = ifelse(year < 1880, 1880,
											 ifelse(year > 2012, 2012,
											 			 year))) %>%
	gender_df(name_col = "first_name",
						year_col = "year") %>%
	mutate(first_name = name)

# join onto full table, since the gender fn only returns unique names
keywords_df_clean_gender_all_names <-
	keywords_df_clean %>%
	left_join(keywords_df_clean_gender,
						by = c('first_name' = 'first_name',
									 'year_rounded' ='year_min'))

odmp_age_gender_names_locations <-
	keywords_df_clean_gender_all_names %>%
	select(-`Incident Details`,
				 -first_name,
				 -other_name1,
				 -other_name2,
				 -other_name3,
				 -year_rounded,
				 -proportion_male,
				 -proportion_female,
				 -year_max) %>%
	mutate(rank = str_replace_all(name_and_rank, name.x, "")) %>%
	mutate(name = name.x) %>%
	select(-name.x,
				 -name.y) %>%
	# age is a bit messy, get only the numbers
	mutate(Age = as.numeric(str_extract(Age, "\\d*")))

write_csv(odmp_age_gender_names_locations,
					"odmp_profile_page_data_1791_2018.csv")

# check the locations

# quick look at a map
ggplot(odmp_age_gender_names_locations,
			 aes(long, lat)) +
	geom_point() +
	coord_map()

library(mapview)
library(sf)

# make a spatial features object for mapping
keywords_df_clean_sf <-
	odmp_age_gender_names_locations %>%
	filter(!is.na(lat)) %>%
	filter(!is.na(long)) %>%
	st_as_sf(coords = c("long", "lat"))

library(leaflet)

m <- leaflet(keywords_df_clean_sf) %>%
	addTiles() %>%  # Add default OpenStreetMap map tiles
	addMarkers(clusterOptions = markerClusterOptions(),
						 popup = paste("Name:", keywords_df_clean_sf$name_and_rank, "<br>",
						 							"Age:", keywords_df_clean_sf$Age, "<br>",
						 							"Gender:", keywords_df_clean_sf$gender, "<br>",
						 							"EOW:", keywords_df_clean_sf$eow, "<br>",
						 							"Tour:", keywords_df_clean_sf$Tour, "<br>",
						 							"Cause:", keywords_df_clean_sf$Cause, "<br>",
						 							"Offender:", keywords_df_clean_sf$Offender))
m  # Print the map





#-------------------------------------------------------------------------
# some plots..

library(tidyverse)
theme_set(theme_minimal(base_size = 13))
odmp <- read_csv("odmp_data_1791_2018.csv")

# how many per year?
n_per_year <-
  ggplot(odmp,
         aes(year)) +
  geom_bar() +
  geom_text(data = odmp %>% count(year, sort = TRUE) %>% slice(1),
            aes(year,
                n + 10,
                label = paste0("Maximum in ", year))) +
  ylab("Police deaths per year")

# what ranks?
n_ranks <-
  odmp %>%
  group_by(Rank) %>%
  tally(sort = TRUE) %>%
  filter(!is.na(Rank)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(Rank, n),
             n)) +
  geom_col()  +
  coord_flip() +
  xlab("")

# what cause?
n_cause <-
  odmp %>%
  group_by(Cause) %>%
  tally(sort = TRUE) %>%
  filter(!is.na(Cause)) %>%
  slice(1:20) %>%
  ggplot(aes(reorder(Cause, n),
             n)) +
  geom_col()  +
  coord_flip() +
  xlab("")

# where are the most and least?
library(statebins)
library(viridis)
n_where <-
  odmp %>%
  group_by(State) %>%
  tally() %>%
  ggplot(aes(state=State,
             fill=n)) +
  geom_statebins(radius = unit(0.5, "npc")) +
  coord_equal() +
  scale_fill_viridis() +
  xlab(paste0("Total number of Police deaths, ",
              min(odmp$year, na.rm = TRUE),
              " - ",
              max(odmp$year, na.rm = TRUE))) +
  theme_statebins()

# proportion of deaths that are heart attacks over time
prop_heart_attack <-
  odmp %>%
  mutate(Cause = if_else(Cause == 'Heart attack',
                         Cause,
                         'other')) %>%
  group_by(year,
           Cause) %>%
  tally() %>%
  mutate(prop = prop.table(n)) %>%
  filter(Cause == 'Heart attack') %>%
  ggplot(aes(year,
             prop,
             group = 1)) +
  geom_point(size = 2) +
  ylab("Proportion of Police deaths\neach year due to heart attack")

# devtools::install_github("thomasp85/patchwork")
library(patchwork)
p_full <-
  ( n_ranks | n_cause | prop_heart_attack) /
  ( n_per_year | n_where )

wrap_elements(p_full) + ggtitle('Police Deaths in the US',
                                subtitle = "Data from https://www.odmp.org, code at https://gist.github.com/benmarwick/caae664abf667012531659cf05055673")

#------

# try facial analysis for sex and gender

# scrape each page for each year
img_srcs <-
map(odmp_urls,
    ~read_html(.x) %>%
      html_nodes(".fixed-width-100 .officer-img") %>%
      html_attr('src'))

# exclude urls that don't have photos
img_srcs_with_photos <-
img_srcs %>%
  unlist() %>%
  enframe() %>%
  filter(!str_detect(value, "no-photo")) %>%
  filter(!str_detect(value, "k9"))


# ket keys for Microsoft Face API (only need to do this once)
# https://portal.azure.com/#create/hub -> AI + Cognitive Services -> ...
# free account allows 30,000 transactions at 20/min
# https://azure.microsoft.com/en-us/try/cognitive-services/my-apis/?apiSlug=face-api&country=UnitedStates&allowContact=true

library(httr)

#----
image_url = img_srcs_with_photos$value #"https://www.odmp.org/media/image/officer/5336/orig/night-watchman-john-gates.jpg"

# DON'T SHARE THESE - DON'T GIT COMMIT THESE
key1 <- "2ed0b49038054fd9a0b86e8a8c1998c4"
key2 <- "44cbe313056f4ed58a0543fab0391724"


end.point <- "https://westus.api.cognitive.microsoft.com/face/v1.0/detect"

# need to throttle this bak to 20/min
APIresponse <-
  map(image_url, ~ POST(url = end.point,
                   content_type('application/json'),
                   add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key1)),
                   body=list(url = .x),
                   query = list(returnFaceAttributes = "age,gender"),
                   encode = "json"))

face_data <-
  map(APIresponse, ~as.data.frame(content(.x))) %>%
  map2(.,  image_url, ~mutate(.x, image_url = .y)) %>%
  bind_rows()

#---------------------------------------------------------

# How to classify into ethnicity?

library(magick)
imgs <- image_url

# get photos from the website
photos <- image_read(imgs)

# save as images
for(i in seq_len(length(photos))){
    image_write(photos[i],
            path = str_extract(imgs[i], "([^/]*)$"),
            format = "jpeg")
}

# how many images do we have?
files <-  list.files(pattern = "jpg$")

# extract faces
# Run Python script to detect faces, draw rectangles, return new image
# this will loop over all jpg in the current wd and
# make new images of only the faces that it finds in the photos
system('python opencv.py')

# convert to greyscale, make same size, cluster.

#--------------------------------------------------

# convert all images to grayscale

library(tidyverse)
library(fs)
library(magick)
library(glue)

train_african_american <- dir_ls("tmp/detected-faces/training/train-african-american")
train_african_american_greyscale <-
	map(train_african_american,
			~image_read(.x) %>%
				image_convert(type = 'grayscale'))

train_caucasian <- dir_ls("tmp/detected-faces/training/train-caucasian")
train_caucasian_greyscale <-
	map(train_caucasian,
			~image_read(.x) %>%
				image_convert(type = 'grayscale'))

train_asian <- dir_ls("tmp/detected-faces/training/train-asian")
train_asian_greyscale <-
	map(train_asian,
			~image_read(.x) %>%
				image_convert(type = 'grayscale'))

test_faces <- dir_ls("tmp/detected-faces/test")
test_greyscale <-
	map(test_faces,
			~image_read(.x) %>%
				image_convert(type = 'grayscale'))

train_dir

fnames <- glue('asian_{1:length(train_asian_greyscale)}.png')
file_move(train_asian, glue('tmp/detected-faces/training/{fnames}'))

fnames <- glue('african_american_{1:length(train_african_american_greyscale)}.png')
file_move(train_african_american, glue('tmp/detected-faces/training/{fnames}'))

fnames <- glue('caucasian_{1:length(train_caucasian_greyscale)}.png')
file_move(train_caucasian, glue('tmp/detected-faces/training/{fnames}'))

train_dir <- 'C:/Users/bmarwick/Desktop/killedbypolice/tmp/detected-faces/training'
validation_dir <- 'C:/Users/bmarwick/Desktop/killedbypolice/tmp/detected-faces/validation'

# prepare model, following https://tensorflow.rstudio.com/blog/keras-image-classification-on-small-datasets.html

library(keras)
# install_keras() # done


#  instantiate the inception_v3 model.
conv_base <- application_inception_v3(
	weights = "imagenet",
	include_top = FALSE,
	input_shape = c(299, 299, 3)
)

# add a model (like conv_base) to a sequential model
model <- keras_model_sequential() %>%
	conv_base %>%
	layer_flatten() %>%
	layer_dense(units = 256, activation = "relu") %>%
	layer_dense(units = 1, activation = "sigmoid")

freeze_weights(conv_base)

# do augumentation to show model more variety in the training set
train_datagen = image_data_generator(
	rescale = 1/255,
	rotation_range = 40,
	width_shift_range = 0.2,
	height_shift_range = 0.2,
	shear_range = 0.2,
	zoom_range = 0.2,
	horizontal_flip = TRUE,
	fill_mode = "nearest"
)

# train our model using the image data generator:
test_datagen <- image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(
	train_dir,                  # Target directory, must be nested with classes
	train_datagen,              # Data generator
	target_size = c(299, 299),  # Resizes all images to 299 × 299
	batch_size = 20,
	class_mode = "binary"       # binary_crossentropy loss for binary labels
)

validation_generator <- flow_images_from_directory(
	validation_dir,
	test_datagen,
	target_size = c(150, 150),
	batch_size = 20,
	class_mode = "binary"
)

model %>% compile(
	loss = "binary_crossentropy",
	optimizer = optimizer_rmsprop(lr = 2e-5),
	metrics = c("accuracy")
)

history <- model %>% fit_generator(
	train_generator,
	steps_per_epoch = 100,
	epochs = 30,
	validation_data = validation_generator,
	validation_steps = 50
)


