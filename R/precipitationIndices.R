#' precipitation threshold index
#'
#' @description Calculates a set of daily precipitation related indices as
#' they were defined for the KNMI14 scenarios brochure
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
#' @param index      vector of thresholds
#' @param scenario   scenario ("GL", "GH", "WL", "WH"). If scenario is not one of the 4
#'                   then the indices are calculated for the reference period 1981-2010
#' @param horizon    time horizon ( DEFAULT=2030, 2050, 2085). If horizon is not one of the 3
#'                   then the indices are calculated for the reference period 1981-2010
#' @param season     season (0= year, 1=winter, 2=spring, 3=summer, 4=autumn)
#' @param subscenario subscenario for extreme precipitation
#' ("lower", "centr" (=DEFAULT), "upper")
#' @export
PrecipThreshIndices<- function(input, index, scenario,
                               horizon = 2030, season,
                               subscenario = "centr") {

  StationSub <- as.character(fread(system.file("refdata","P102.txt",
                                  package = "scenarioIndices"))$V1)

  if (class(input) != "knmiTF") {
    input <- ReadInput("rr", input)
  }

  if (!scenario %in% c("GL","GH","WL","WH") &&
      horizon !=c(2030,2050,2085)){
    input <- input$obs
    dt <- input$date
    input <- input[, StationSub]
  } else {
    input <- TransformPrecip(input = input, scenario=scenario,
                             horizon=horizon, subscenario = subscenario)
    dt <- input[-c(1:5), V1] #nolint
    stationID         <- input[(1)]
    names(input) <- as.character(stationID)
    input <- input[-c(1:5), StationSub, with = FALSE]
  }

  input <- as.data.frame(input)

  seasonalSplit <- SeasonalSplit(season, dt)
  id  <- seasonalSplit$id
  idy <- seasonalSplit$idy
  yy  <- dt %/% 10000

  #Indices
  switch(index,
         "N0.1mm" = X <- apply(input[id,-1]>=0.1,2,sum) / length(unique(yy)),
         "N0.5mm" = X <- apply(input[id,-1]>=0.5,2,sum) / length(unique(yy)),
         "N10mm" = X <- apply(input[id,-1]>=10,2,sum) / length(unique(yy)),
         "N20mm" = X <- apply(input[id,-1]>=20,2,sum) / length(unique(yy)),
         "N30mm" = X <- apply(input[id,-1]>=30,2,sum) / length(unique(yy)),
         "SeasMean" = X <-  apply(aggregate(input[id,-1],by=list(idy),  sum)[,-1],2, mean),
         "SeasSD" = X <-  apply(aggregate(input[id,-1],by=list(idy),  sum)[,-1],2, sd))

    return(X)
}

#' Calculates a set of precipitation related indices (thresholds) for all scenarios, horizons, and
#' seasons at once
#'
#' @description Calculates a set of daily precipitation related indices as
#'   they were defined for the KNMI14 scenarios brochure
#' @inheritParams PrecipThreshIndices
#' @export
PrecIndicesWrapper <- function(input, subscenario = "centr") {

  if (class(input) != "knmiTF") {
    input <- ReadInput("rr", input)
  }

  fn <- function(index, season, scenario, horizon) {
    tmp <- PrecipThreshIndices(input = input, index = index,
                               ofile = ofile, scenario = scenario,
                               horizon = horizon, season = season,
                               subscenario = subscenario)
    tmp          <- as.data.frame(t(tmp))
    tmp$season   <- season
    tmp$horizon  <- horizon
    tmp$subscenario <- subscenario
    tmp$scenario <- scenario
    tmp$index    <- index
    tmp
  }

  indices <- c("N0.1mm", "N0.5mm", "N10mm", "N20mm", "N30mm")

  combinations <- MakeCombinations(indices)

  result <- pmap(combinations, fn)

  result <- rbindlist(result)

  # hasErrors <- map_lgl(result, function(x) !is.null(x$error))

  # result <- map_df(result[!hasErrors], "result")
  #
  # result <- as.data.table(result)
  nCols <- ncol(result)
  names <- colnames(result)
  setcolorder(result, names[c( (nCols - (0:4)), 1, 2 : (nCols-5))])
  result
  # combinations[hasErrors, ]
}
