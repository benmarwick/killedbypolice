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
#' by the police in the US during 2013-2017.
#'
#' @format A data frame with 5,578 rows and 11 variables:
#' \describe{
#'   \item{date_format}{date, formatted for computation}
#'   \item{State}{State location}
#'   \item{deceased_name}{State location}
#'   \item{deceased_age}{Age at death}
#'   \item{gender}{Gender}
#'   \item{race}{Race}
#'   \item{method_1}{Method of killing, "G" = "Gun", "T" = "Taser", "R" = "Restraint/Physical Force", "C" = "Chemical", "V" = "Vehicle", "O" = "Other"}
#'   \item{method_2}{Method of killing}
#'   \item{method_3}{Method of killing}
#'   \item{method_4}{Method of killing}
#'   \item{method_5}{Method of killing}
#' }
#' @source \url{http://killedbypolice.net/}
"kbp2013_2017"
