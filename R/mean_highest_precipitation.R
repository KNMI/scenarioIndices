#' Calculate the mean highest precipitation deficit during Growing Season (GS) (April-August)
#' as it was calculated for KNMI14 scenarios brochure
#' this index uses the evmk of DeBilt and precipitation amount of 102 stations (the homogenenised ones) over the NL
#' @description      function reads reference ts for evmk, rr, tg & rdrs
#' @param ifile_tg   input file for tg
#' @param ifile_rsds input file for rsds
#' @param ifile_rr   input for rr
#' @param ofile      (DEFAULT="uitvoer.txt") Name of the output file to write the transformed data to.
#'                    Format is similar to ifile
#' @param scenario    scenario                      ["GL", "GH", "WL", "WH"]
#' @param horizon     time horizon                  [2030 (=DEFAULT), 2050, 2085]
#' @param regio.file  this (optional) argument provides the name of an ASCII file that relates the stations to
#' a particular region. First column is station id and second column region
#'                KNMI14 distinguishes following regions:
#'                <NLD> Nederland [DEFAULT]
#'                <NWN> Noordwest Nederland
#'                <ZWN> Zuidwest Nederland
#'                <NON> Noordoost Nederland
#'                <MON> Middenoost Nederland
#'                <ZON> Zuidoost Nederland
#' @export
PrecipDeficit_sce<- function(ifile_tg, ifile_rsds, ifile_rr,
                               ofile="uitvoer.txt",
                               scenario,
                               horizon = NA, regio.file = NA) {


  StationSub <- as.character(fread(system.file("refdata","P102.txt", package = "scenarioIndices"))$V1)

  # read PrecipDeficitRef
# Wrong PrecipDeficitRef
  PrecipDeficitRef <- system.file("refdata", "PrecipDeficitRef_19810101-20101231_error.txt", package = "scenarioIndices")
  Xstat <- read.table(PrecipDeficitRef)$V2
  nx <- length(Xstat)
  Xstat <- Xstat[1:(nx-1)]
# Correct PrecipDeficitRef
 # Xstat <- fread(system.file("refdata","PrecipDeficitRef_19810101-20101231_correct.txt", package = "scenarioIndices"))

  #input for scenarios
  #calculate evmk for scenarios
  evmk_scenario <- TransformEvap(ifile_tg = ifile_tg,
                                                   ifile_rsds = ifile_rsds,
                                                   ofile="uitvoer.txt",
                                                   scenario = scenario,
                                                   horizon = horizon,
                                                   regio.file = regio.file)

  dt     <- evmk_scenario[-(1:5),1, with = FALSE]
  mm     <- (dt%/%100)%%100
  amjjas <- which(mm>=4 & mm<=9)
  yy     <- (dt%/%10000)[amjjas]

  stationID           <- evmk_scenario[(1)]
  names(evmk_scenario)<- as.character(stationID)
  evDeBiltSC          <- evmk_scenario[-(1:5),"260",with=FALSE]
  evDeBiltSCGS        <- evDeBiltSC[amjjas]

  # calculate rr for scenarios
  rrScenario <- TransformPrecip(ifile = ifile_rr,
                  ofile="tmp.txt",
                  scenario=scenario,
                  horizon = horizon,
                  subscenario="centr", rounding = TRUE)

  stationID         <- rrScenario[(1)]
  names(rrScenario) <- as.character(stationID)
  #Wrong version. Correct should be -(1:5)
  rrScenario        <- rrScenario[-(1:6),StationSub, with=FALSE]
  rrScenarioMean    <- apply(as.data.frame(rrScenario[amjjas,]),1,mean)


  # maximum potential precipitation deficit for scenarios & statistics (mean, sd, ranks)
  Ysum       <- tapply(evDeBiltSCGS$`260` - rrScenarioMean, yy, max.pos.cumsum)
  N<- 3 # find 3 highest years
  ndy        <- order(Ysum,decreasing=T)[1:N]
  highestsce <- round(mean(Ysum[ndy]),1)
  Ystat      <- c(mean(Ysum), sd(Ysum), sort(as.numeric(Ysum)))
  delta      <- 100*(Ystat-Xstat) / Xstat
  nd <- length(delta)
  highestdel <- round(mean(delta[(nd-2):nd]),1)

  table_sce  <- data.frame(year = c("mean","sd",names(sort(Ysum)), "high"),
                          values = c(round(Ystat,1), highestsce),
                          relativechange = c(round(delta,1), highestdel))

  names(table_sce) <- c("year",  paste(scenario,horizon,sep=""), "delta")

  write.table(format(table_sce,width=8,nsmall=2), ofile,col.names=F,row.names=F,quote=F)

  return(table_sce)
}
