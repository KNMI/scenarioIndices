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

  if (!scenario %in% c("GL","GH","WL","WH") && horizon !=c(2030,2050,2085)){
    input <-  knmitransformer::ReadInput("tn",
        system.file("refdata","KNMI14____ref_tn___19810101-20101231_v3.2.txt",
                    package="knmitransformer"))$obs
  } else {
    input <- TransformTemp(input=input, ofile=NA, scenario=scenario,
                           horizon=horizon, var="tn", regions = regions)
    input <- input[-c(1:5)]

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
