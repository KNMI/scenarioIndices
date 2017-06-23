#' Calculate the mean highest precipitation deficit during Growing Season (GS) (April-August)
#' as it was calculated for KNMI14 scenarios brochure For Just the REFERENCE Period
#' this index uses the evmk of DeBilt and precipitation amount of 102 stations (the homogenenised ones) over the NL
#' @description      function reads reference ts for evmk, rr, tg & rdrs
#' @param ofile      (DEFAULT="uitvoer.txt") Name of the output file to write the transformed data to.
#'                    Format is similar to input
PrecipDeficitRef <- function(ofile = NA) {


  StationSub <- as.character(fread(system.file("refdata",
      "P102.txt", package = "scenarioIndices"))$V1)

  # reference for evmk for de bilt
  evmkRef <- fread(system.file("refdata","KNMI14____ref_evmk___19810101-20101231_v3.2.txt",
                               package="knmitransformer"))
  stationID      <- evmkRef[(1)]
  names(evmkRef) <- as.character(stationID)
  evDeBilt       <- evmkRef[-c(1:5),"260",with=FALSE]

  dt         <- evmkRef[-c(1:5),1, with = FALSE]
  mm         <- (dt%/%100)%%100
  amjjas     <- which(mm>=4 & mm<=9)
  yy         <- (dt%/%10000)[amjjas]
  evDeBiltGS <- evDeBilt[amjjas]

  # rr reference for P102
  rrRef       <- fread(system.file("refdata",
      "KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt",
      package = "scenarioIndices"))
  stationID   <- rrRef[(1)]
  names(rrRef)<- as.character(stationID)
# Error in KNMI14 runs: 19810101 was removed from calculations
# CORRECT
#  rrRef       <- rrRef[-(1:5),StationSub, with=FALSE]
#
# WRONG
  rrRef       <- rrRef[-c(1:6),StationSub, with=FALSE]
#
  rrRefMean   <- apply(as.data.frame(rrRef[amjjas,]),1,mean)

  # maximum potential precipitation deficit & statistics (mean, sd, ranks)
  Xsum  <- tapply(evDeBiltGS$`260` - rrRefMean,yy,max.pos.cumsum)
  N<- 3 # find 3 highest years
  ndx <- order(Xsum,decreasing=T)[1:N]
  highestref <- mean(Xsum[ndx])
  Xstat <- c(mean(Xsum),sd(Xsum),sort(as.numeric(Xsum)))# mean,sd,ranks Xsum

  table_ref <- data.frame(year = c("mean","sd", names(sort(Xsum)), "high"),
                          reference = c(round(Xstat,1), round(highestref),1))


  return(table_ref)
}
