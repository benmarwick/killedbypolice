# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



#' Details of people killed by police in the US during 2013-2017
#'
#' A dataset containing basic information of people killed
#' by the police in the US during 2013-2017. Collected on
#' January 2018.
#'
#' @format A data frame with 5,578 rows and 11 variables:
#' \describe{
#'   \item{event_date}{full date, formatted for computation}
#'   \item{event_year}{year of event}
#'   \item{event_month}{month of event}
#'   \item{event_day}{day of month of event}
#'   \item{state}{US state location}
#'   \item{name}{State location}
#'   \item{age}{Age at death of deceased}
#'   \item{gender}{Gender of deceased, "M" = Male, "F" = Female, "T" = Transgender}
#'   \item{race_ethnicity}{Race/ethnicity of deceased, "A" = Asian; "B" = Black; "I" = Native American; "L" = Latina/o; "O" = Other; "PI" = Pacific Islander; "W" = White. Note that \url{http://killedbypolice.net/} does not give a key for these codes, so we infer them from the news reports.}
#'   \item{method_1}{Method of killing, from the key at\url{http://killedbypolice.net/}:  "G" = Gun; "T" = Taser; "R" = Restraint/Physical Force; "C" = Chemical; "V" = Vehicle; "O" = Other}
#'   \item{method_2}{Method of killing, where there are multiple in one event}
#'   \item{method_3}{Method of killing}
#'   \item{method_4}{Method of killing}
#'   \item{method_5}{Method of killing}
#' }
#' @source \url{http://killedbypolice.net/}
"kbp2013_2017"
