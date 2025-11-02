#' Southeastern Australia Bushfire Weather and Drivers (1979-2019)
#'
#' A dataset containing key fire weather indices (FWI, MSR), their
#' climate drivers (temperature, precipitation, IOD, SAM), and the
#' resulting burned area for Southeastern Australia from 1979 to 2019.
#'
#' @format A data frame with 41 observations and 8 variables:
#' \describe{
#'   \item{year}{The observation year (1979-2019)}
#'   \item{fwi_sm}{The highest 7-day mean FWI (FWI7x-SM)}
#'   \item{msr_sm}{The maximum Monthly Severity Rating (MSR-SM)}
#'   \item{tmax}{Annual maximum 7-day mean maximum temperatures (TX7x) }
#'   \item{precip_total}{Total precipitation during the fire season (Sep-Feb)}
#'   \item{iod_mean}{Mean Indian Ocean Dipole (IOD) index for Sep-Dec}
#'   \item{sam_mean}{Mean Southern Annular Mode (SAM) index for Sep-Dec}
#'   \item{area_burned}{Total fire season burned area (MODIS, km2)}
#' }
#' @source \url{https://climexp.knmi.nl/bushfires_timeseries.cgi}
"ausbushfire_data"
