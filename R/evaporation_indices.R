#' Calculate potential evaporation (Makkink), year mean (seasons sum) and summer for brochure validation
#' @description Function reads transormed mean temperature and transformed global radiation
#' and calculates the Makkink evaporation for 'future time series' that match a certain climate, makes season sums and summer values
#' @param inputTemp input file for tg
#' @param inputRad input file for rsds
#' @param ofile          (DEFAULT="uitvoer.txt") Name of the output file to write the transformed data to.
#'                Format is similar to input
#' @param scenario  scenario                      ["GL", "GH", "WL", "WH"]
#' @param horizon   time horizon                  [2030 (=DEFAULT), 2050, 2085]
#' @param regions     vector of regions
#'                   KNMI14 distinguishes following regions:\cr
#'                   <NLD> Nederland (DEFAULT) \cr
#'                   <NWN> Noordwest Nederland \cr
#'                   <ZWN> Zuidwest Nederland \cr
#'                   <NON> Noordoost Nederland \cr
#'                   <MON> Middenoost Nederland \cr
#'                  <ZON> Zuidoost Nederland
#' @export
evmkSumsRelchange <- function(inputTemp, inputRad, scenario,
                                      horizon = NA, regions = "NLD", ofile=NA) {

  flog.info("Running evaporation calculation")
  flog.debug("Version is 1.0")
  # CONSTANTS AND FUNCTIONS #

  if (!horizon %in% c(2030, 2050, 2085)) {
    flog.error("horizon={%s} has to be a valid period", paste(horizon))
  stop("Horizon must be valid, i.e. 2030, 2050, or 2085")
  }

  evmkRef <- fread(KnmiRefFile("KNMI14____ref_evmk___19810101-20101231_v3.2.txt"))

  evmkScenario <- TransformEvap(inputTemp = inputTemp,
                                 inputRad = inputRad,
                                 ofile=NA,
                                 scenario = scenario,
                                 horizon = horizon,
                                 regions = regions)

  if (!all(evmkRef [1:5] == evmkScenario[1:5])) {
    flog.error("Same stations should be used for reference and scenarios")
    stop("Same stations should be used for reference and scenarios")
  }

  evRef <- evmkRef[-c(1:5), ]
  evRef <- as.data.frame(evRef)

  evSce <- evmkScenario[-c(1:5), ]
  evSce <- as.data.frame(evSce)

  dt <- evRef[,1]

  # 5 because of annual & 4 seasons
  tableSce <-  tableRef <- reltable <- as.data.frame(matrix(NA, 5, ncol(evRef)))
  names(tableRef) <- evmkRef[1]
  names(tableSce) <- evmkRef[1]
  names(reltable) <- evmkRef[1]


  i = 0
  # # seasonal variables
  for (season in c("year", "winter", "spring", "summer", "autumn")) {
      i=i+1
      tableRef[i,-1]  <- round(apply(evRef[SeasonalSplit(season,dt)$id,-1],2,sum)/30,0)
      tableSce[i,-1]  <- round(apply(evSce[SeasonalSplit(season,dt)$id,-1],2,sum)/30,0)
  } # end seasonal variables
  # relative change
  reltable[,-1] <- round( (100 * (tableSce[,-1] - tableRef[,-1]) / tableRef[,-1]),2)
  reltable[,1] <- c("year","winter","spring","summer","autumn")

  write.table(format(reltable,width=8,nsmall=2), ofile,col.names=F,row.names=F,quote=F)
  return(reltable)
}
