#' relative change
#' @param input Name of the input file (ASCII) that contains reference data
#'                   (all numerics) in which the columns provide time series for
#'                   specific stations.
#'                   The first column should provide either 00000000 or a
#'                   datestring YYYYMMDD:
#'                   Rows starting with 00000000 are considered station info
#'                   (station number, lat, lon etc.) and are ignored.
#'                   Rows starting with a datestring refer to a specific day in the
#'                   time series.
#'                   Rows starting with "#" are completely ignored and returned
#'                   unchanged.
#' @param scenario   scenario ("GL", "GH", "WL", "WH"). If scenario is not one of the 4
#'                   then the indices are calculated for the reference period 1981-2010
#' @param horizon    time horizon ( DEFAULT=2030, 2050, 2085). If horizon is not one of the 3
#'                   then the indices are calculated for the reference period 1981-2010
#' @param subscenario subscenario for extreme precipitation
#' ("lower", "centr" (=DEFAULT), "upper")
#' @param ofile      (DEFAULT=NA) Name of the output file to write the indices to.
#'                   Format is similar to input without the 5 first lines
#' @export
RelativeChange <- function(input, scenario = NA, horizon = NA, ofile = NA){

    threshold <- c(0.1,0.3,1.0,5.0,10.0,15.0,20.0,30.0)

    input <- system.file("refdata", "KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt",
                          package="knmitransformer")

          obs <- PrecipThreshIndices(input, threshold, secnario ="ref",
                                     horizon = 1981, subscenario = "centr")

          fut <- PrecipThreshIndices(input, threshold, secnario ="WH",
                                     horizon = 1981, subscenario = "centr")
  delta <- obs
delta[,-1]    <- 100 * (fut[,-1] - obs[,-1]) / obs[,-1]

}
