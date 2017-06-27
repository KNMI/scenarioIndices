#' Calculate the mean highest precipitation deficit during Growing Season (GS) (April-August)
#' as it was calculated for KNMI14 scenarios brochure
#' this index uses the evmk of DeBilt and precipitation amount of 102 stations (the homogenenised ones) over the NL
#' @description      function reads reference ts for evmk, rr, tg & rdrs
#' @param inputTemp   input file for tg
#' @param inputRad input file for rsds
#' @param inputPrec   input for rr
#' @param scenario    scenario                      ["GL", "GH", "WL", "WH"]
#' @param horizon     time horizon                  [2030 (=DEFAULT), 2050, 2085]
#' @param regions     vector of regions
#'                   KNMI14 distinguishes following regions:\cr
#'                   <NLD> Nederland (DEFAULT) \cr
#'                   <NWN> Noordwest Nederland \cr
#'                   <ZWN> Zuidwest Nederland \cr
#'                   <NON> Noordoost Nederland \cr
#'                   <MON> Middenoost Nederland \cr
#'                   <ZON> Zuidoost Nederland
#' @export
PrecipDeficit <- function(inputTemp, inputRad, inputPrec,
                        scenario, horizon = 2030, regions = "NLD") {


  StationSub <- as.character(fread(system.file("refdata","P102.txt",
                                               package = "scenarioIndices"))$V1)

  # read PrecipDeficitRef
# Wrong PrecipDeficitRef
  PrecipDeficitRef <- system.file("refdata", "PrecipDeficitRef_19810101-20101231_error.txt",
                                  package = "scenarioIndices")
  Xstat <- read.table(PrecipDeficitRef)$V2
  nx <- length(Xstat)
  Xstat <- Xstat[1:(nx-1)]

# Correct PrecipDeficitRef
 # Xstat <- fread(system.file("refdata",
  # "PrecipDeficitRef_19810101-20101231_correct.txt", package = "scenarioIndices"))

  #input for scenarios
  #calculate evmk for scenarios
  evmkScenario <- TransformEvap(inputTemp = inputTemp, inputRad = inputRad,
                                  scenario = scenario, horizon = horizon,
                                 regions = "NLD")

  dt     <- evmkScenario[-c(1:5),1, with = FALSE]
  mm     <- (dt%/%100)%%100
  amjjas <- which(mm>=4 & mm<=9)
  yy     <- (dt%/%10000)[amjjas]

  stationID           <- evmkScenario[(1)]
  names(evmkScenario)<- as.character(stationID)
  evDeBiltSC          <- evmkScenario[-c(1:5),"260",with=FALSE]
  evDeBiltSCGS        <- evDeBiltSC[amjjas]

  # calculate rr for scenarios
  rrScenario <- TransformPrecip(input = inputPrec,
                  ofile=NA,
                  scenario=scenario,
                  horizon = horizon,
                  subscenario="centr", rounding = TRUE)

  stationID         <- rrScenario[(1)]
  names(rrScenario) <- as.character(stationID)
  #Wrong version. Correct should be -(1:5)
  rrScenario        <- rrScenario[-c(1:6),StationSub, with=FALSE]
  rrScenarioMean    <- apply(as.data.frame(rrScenario[amjjas,]),1,mean)


  # maximum potential precipitation deficit for scenarios & statistics (mean, sd, ranks)
  Ysum       <- tapply(evDeBiltSCGS$`260` - rrScenarioMean, yy, max.pos.cumsum)
  N<- 3 # find 3 highest years
  ndy        <- order(Ysum,decreasing=T)[1:N]
  highestsce <- mean(Ysum[ndy])
  Ystat      <- c(mean(Ysum), sd(Ysum), sort(as.numeric(Ysum)))
  delta      <- 100 * (Ystat-Xstat) / Xstat
  nd <- length(delta)
  highestdel <- mean(delta[(nd-2):nd])

  table_sce  <- data.frame(year = c("mean","sd",names(sort(Ysum)), "high"),
                          values = c(round(Ystat,1), round(highestsce,1)),
                          relativechange = c(round(delta,1), round(highestdel,1)))

  names(table_sce) <- c("year",  paste(scenario,horizon,sep=""), "delta")

  return(table_sce)
}
