#' Calculates a set of TX related indices
#'
#' @description Calculates a set of daily maximum temperature related indices as
#' they were defined for the KNMI14 scenarios brochure
#' @param index      indices ("nID", "nWD", "nSD", "nTD", "aTX")
#' @inheritParams TempAvgIndices
#' @export
TempMaxIndices <- function(input, index,
                          ofile = NA, scenario,
                          horizon=2030, season,
                          regions = "NLD") {


  if (!index %in% c("nID", "nWD", "nSD", "nTD", "aTX")){
    stop("index should be one of nID nWD nSD nTD aTX")
  }


  if (!scenario %in% c("GL","GH","WL","WH") && horizon !=c(2030,2050,2085)){
    input <-  knmitransformer::ReadInput("tx",
         system.file("refdata","KNMI14____ref_tx___19810101-20101231_v3.2.txt",
                                      package="knmitransformer"))$obs
  } else {
    input <- TransformTemp(input=input, ofile=NA, scenario=scenario,
                           horizon=horizon, var="tx", regions = regions)
    input <- input[-(1:5) ]

  }

  input <- as.data.frame(input)

  #Seasons
  mm <- (input[,1]%/%100)%%100
  ss <- as.integer((mm/3)%%4+1)
  yy <-  input[,1]%/%10000
  wy <- ifelse(mm<12,yy,yy+1)

  if(season=="year"){
    id  <- 1:length(yy)
    idy <- yy
  } else {
    if(season=="winter"){
      id  <- which(ss==1 & wy > min(wy) & wy < max(wy))
      idy <- wy[id]
    } else {
      id  <- which(ss==season)
      idy <- yy[id]
    }
  }

  #Indices
  switch(index,
         "nID" = X <- aggregate(input[id,-1] < 0,by=list(idy),  sum),
         "nWD" = X <- aggregate(input[id,-1] >= 20,by=list(idy),  sum),
         "nSD" = X <- aggregate(input[id,-1] >= 25,by=list(idy),  sum),
         "nTD" = X <- aggregate(input[id,-1] >= 30,by=list(idy),  sum),
         "aTX" = X <- aggregate(input[id,-1],by=list(idy),  mean))

  return(X)
}




