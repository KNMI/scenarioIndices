#' Calculate precipitation threshold indices
#' @description Calculate precipitation threshold indices
#' @param input input file with precipitation
#' @param index vector threshold indices
#' @param season vector year,winter, spring, summer, autumn
#' @export
PrecipThreshIndices <- function(input, index, season) {
  input <- as.data.frame(input)

  StationSub <- as.character(fread(system.file("refdata","P102.txt",
                                               package = "scenarioIndices"))$V1)

  dt    <- input[, 1]
  input <- input[, c("date", StationSub)]

  seasonalSplit <- SeasonalSplit(season, dt)
  id  <- seasonalSplit$id
  idy <- seasonalSplit$idy
  yy  <- dt %/% 10000

  #Indices
  switch(index,
         "N0.1mm" = X <- apply(input[id,-1]>=0.1,2,sum) / length(unique(yy)),
         "N0.3mm" = X <- apply(input[id,-1]>=0.3,2,sum) / length(unique(yy)),
         "N1mm" = X <- apply(input[id,-1]>=1,2,sum) / length(unique(yy)),
         "N5mm" = X <- apply(input[id,-1]>=5,2,sum) / length(unique(yy)),
         "N10mm" = X <- apply(input[id,-1]>=10,2,sum) / length(unique(yy)),
         "N15mm" = X <- apply(input[id,-1]>=15,2,sum) / length(unique(yy)),
         "N20mm" = X <- apply(input[id,-1]>=20,2,sum) / length(unique(yy)),
         "N30mm" = X <- apply(input[id,-1]>=30,2,sum) / length(unique(yy)),
         "Psum" = X <-  apply(aggregate(input[id,-1],by=list(idy),  sum)[,-1],2, mean),
         "sdPsum" = X <-  apply(aggregate(input[id,-1],by=list(idy),  sum)[,-1],2, sd))

  return(X)
}

#' Wrapper function for precipitation indices
#' @description Wrapper function for precipitation indices
#' @param input precipitation input file
#' @export
PrecIndicesWrapper <- function(input) {


  fn <- function(index, season) {
    tmp <- PrecipThreshIndices(input = input, index = index, season = season)
    tmp          <- as.data.frame(t(tmp))
    tmp$season   <- season
    tmp$index    <- index
    tmp
  }

  indices <- c("N0.1mm", "N0.3mm", "N1mm", "N5mm", "N10mm", "N15mm", "N20mm",
               "N30mm", "Psum", "sdPsum")
  seasons <- c("year", "winter", "spring", "summer", "autumn")

  combinations <- expand.grid(index = indices, season = seasons, stringsAsFactors = FALSE)

  result <- pmap(combinations, fn)

  result <- rbindlist(result)

  # hasErrors <- map_lgl(result, function(x) !is.null(x$error))

  # result <- map_df(result[!hasErrors], "result")
  #
  # result <- as.data.table(result)
  nCols <- ncol(result)
  names <- colnames(result)
  setcolorder(result, names[c( (nCols - (0:1)), 1, 2 : (nCols-2))])
  result
  # combinations[hasErrors, ]
}
