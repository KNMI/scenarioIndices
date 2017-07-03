#' Calculates a set of TN related indices
#'
#' @description Calculates a set of daily minimum temperature related indices as
#' they were defined for the KNMI14 scenarios brochure
#' @param index      indices ("nstFD", "nFD", "nTN", "aTN")
#' @inheritParams TempAvgIndices
#' @export
TempMinIndices<- function(input, index,
                          ofile = NA, scenario,
                          horizon=2030, season,
                          regions = "NLD") {

  if (!index %in% c("nstFD", "nFD", "nTN", "aTN")){
    stop("index should be one of nstFD nFD nTN aTN")
  }

  if (class(input) != "knmiTF") {
    input <- ReadInput("tn", input)
  }

  if (!scenario %in% c("GL", "GH", "WL", "WH") &&
      horizon != c(2030, 2050, 2085)) {
    input <- input$obs
  } else {
    input <- TransformTemp(input = input, ofile = NA, scenario=scenario,
                           horizon = horizon, var = "tn", regions = regions)
    input <- input[-c(1:5), ]
  }

  input <- as.data.frame(input)

  seasonalSplit <- SeasonalSplit(season, input[, 1])
  id  <- seasonalSplit$id
  idy <- seasonalSplit$idy

  #Indices
  switch(index,
         "nstFD" = X <- aggregate(input[id, -1] < -10, by=list(idy),  sum),
         "nFD"   = X <- aggregate(input[id, -1] < 0,   by=list(idy),  sum),
         # in the brochure they are mentioned as warm nights
         "nTN"   = X <- aggregate(input[id, -1] >= 20, by=list(idy),  sum),
         "aTN"   = X <- aggregate(input[id, -1],       by=list(idy),  mean))
  return(X)
}

#' Calculates a set of TN related indices for all scenarios, horizons, and
#' seasons at once
#'
#' @description Calculates a set of daily minimum temperature related indices as
#' they were defined for the KNMI14 scenarios brochure
#' @inheritParams TempAvgIndices
#' @export
TempMinIndicesWrapper <- function(input, regions = "NLD", ofile = NA) {

  if (class(input) != "knmiTF") {
    input <- ReadInput("tn", input)
  }

  fn <- function(index, season, scenario, horizon) {
    tmp <- TempMinIndices(input = input, index = index, ofile = ofile,
                          scenario = scenario, horizon = horizon, season = season,
                          regions = regions)
    tmp$season   <- season
    tmp$horizon  <- horizon
    tmp$scenario <- scenario
    tmp$index    <- index
    tmp
  }


  indices <- c("nstFD", "nFD", "nTN", "aTN")

  combinations <- MakeCombinations(indices)

  result <- pmap(combinations, fn)
  result <- rbindlist(result)
  setnames(result, "Group.1", "year")
  nCols <- ncol(result)
  names <- colnames(result)
  setcolorder(result, names[c( (nCols - (0:3)), 1, 2 : (nCols-4))])
  result
}
