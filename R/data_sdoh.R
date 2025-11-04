#' @title 2020 Social Determinants of Health (SDOH) Data
#'
#' @description Subset of data from the 2020 Social Determinants of 
#' Health (SDOH) Database. For more information about the 2020 SDOH Database,
#' visit: \url{www.ahrq.gov/sdoh/index.html}.
#'
#' @format A tibble with 3,229 rows and 29 columns:
#' \describe{
#' \item{YEAR}{SDOH file year}
#' \item{COUNTYFIPS}{State-county FIPS Code (5-digit)}
#' \item{STATEFIPS}{State FIPS Code (2-digit)}
#' \item{STATE}{State name}
#' \item{COUNTY}{County name}
#' \item{REGION}{Census region name}
#' \item{TERRITORY}{Territory indicator (1= U.S. Territory, 0= U.S. State or DC)}
#' \item{ACS_PCT_AGE_0_4}{Percentage of population between ages 0-4}
#' \item{ACS_PCT_AGE_5_9}{Percentage of population between ages 5-9}
#' \item{ACS_PCT_AGE_10_14}{Percentage of population between ages 10-14}
#' \item{ACS_PCT_AGE_15_17}{Percentage of population between ages 15-17}
#' \item{NOAAC_PRECIPITATION_JAN}{Monthly (January) precipitation (Inches)}
#' \item{NOAAC_PRECIPITATION_FEB}{Monthly (February) precipitation (Inches)}
#' \item{NOAAC_PRECIPITATION_MAR}{Monthly (March) precipitation (Inches)}
#' \item{NOAAC_PRECIPITATION_APR}{Monthly (April) precipitation (Inches)}
#' \item{NOAAC_PRECIPITATION_MAY}{Monthly (May) precipitation (Inches)}
#' \item{NOAAC_PRECIPITATION_JUN}{Monthly (June) precipitation (Inches)}
#' \item{NOAAC_PRECIPITATION_JUL}{Monthly (July) precipitation (Inches)}
#' \item{NOAAC_PRECIPITATION_AUG}{Monthly (August) precipitation (Inches)}
#' \item{NOAAC_PRECIPITATION_SEP}{Monthly (September) precipitation (Inches)}
#' \item{NOAAC_PRECIPITATION_OCT}{Monthly (October) precipitation (Inches)}
#' \item{NOAAC_PRECIPITATION_NOV}{Monthly (November) precipitation (Inches)}
#' \item{NOAAC_PRECIPITATION_DEC}{Monthly (December) precipitation (Inches)}
#' \item{HHC_PCT_HHA_NURSING}{Percentage of home health agencies offering nursing care services}
#' \item{HHC_PCT_HHA_PHYS_THERAPY}{Percentage of home health agencies offering physical therapy services}
#' \item{HHC_PCT_HHA_OCC_THERAPY}{Percentage of home health agencies offering occupational therapy services}
#' \item{HHC_PCT_HHA_SPEECH}{Percentage of home health agencies offering speech pathology services}
#' \item{HHC_PCT_HHA_MEDICAL}{Percentage of home health agencies offering medical social services}
#' \item{HHC_PCT_HHA_AIDE}{Percentage of home health agencies offering home health aide services}
#' }
"sdoh"
