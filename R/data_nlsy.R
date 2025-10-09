#' @title National Longitudinal Survey of Youth (NLSY) Data
#'
#' @description These data are a subset from the National Longitudinal Survey of Youth
#' (NLSY) 1979 Children and Young Adults.The data contains 2,976 observations and 10
#' variables.
#'
#' For more information about the National Longitudinal Survey of Youth,
#' visit https://www.nlsinfo.org/.
#'
#' @format A tibble with 2,976 rows and 11 columns:
#' \describe{
#' \item{CID}{Child identification number)}
#' \item{race}{race of child (Hispanic, Black, Non-Black,Non-Hispanic)}
#' \item{gender}{gender of child (1 = male, 0 = female)}
#' \item{birthord}{birth order of child}
#' \item{magebirth}{Age of mother at birth of child}
#' \item{bthwht}{whether child was born low birth weight (1 = yes, 0 = no)}
#' \item{breastfed}{whether child was breastfed (1 = yes, 0 = no)}
#' \item{medu}{Highest grade completed by child’s mother}
#' \item{math}{PIAT Math Standard Score}
#' \item{read}{PIAT Reading Recognition Standard Score}
#' \item{hhnum}{Number of household members in household}
#' }
"nlsy"
