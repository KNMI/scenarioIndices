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
  # CONSTANTS AND FUNCTIONS ###############################################################################

  if (!horizon %in% c(2030, 2050, 2085)) {
    flog.error("horizon={%s} has to be a valid period", paste(horizon))
  stop("Horizon must be valid, i.e. 2030, 2050, or 2085")
  }

  evmkRef <- fread(system.file("refdata","KNMI14____ref_evmk___19810101-20101231_v3.2.txt",
                                package = "knmitransformer"))

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

  mm <- (evRef[,1]%/%100)%%100
  ss <- as.integer( (mm/3)%%4+1)
  yy <-  evRef[,1]%/%10000
  wy <- ifelse(mm<12,yy,yy+1)

  products <- data.frame("sum"=1)
  drempels <- vector()

  tableSce <-  tableRef <- reltable <- as.data.frame(matrix(NA,5 * (length(drempels)+sum(products)),ncol(evRef)))
  names(tableRef) <- evmkRef[1]
  names(tableSce) <- evmkRef[1]
  names(reltable) <- evmkRef[1]

  i = 0

  # seasonal variables
  for (season in 0:4) {
    if (season==0) {
      id  <- 1:length(yy)
    } else {
      if (season==1) {
        id  <- which(ss==1 & wy > min(wy) & wy < max(wy))
      } else {
        id  <- which(ss==season)
      }
    }

    if (length(drempels>0)) {
      for (j in 1:length(drempels)) {
        i=i+1
        dname <- paste("N_",drempels[j],"mm",season,sep="")
        tableRef[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname))) # nolint
        tableRef[i,-1] <- apply(evRef[id,-1]>=drempels[j],2,sum) / length(unique(yy))
        ##scenarios
        tableSce[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
        tableSce[i,-1] <- apply(evSce[id,-1]>=drempels[j],2,sum) / length(unique(yy))
      }
    }

    prod="sum"   # sd seizoenssom EV
    if (products[prod]==1) {
      dname=paste(prod,season,sep="")
      i=i+1
      tableRef[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
      #table[i,-1] <- apply(X[,-1]       , 2          ,   sd)
      tableRef[i,-1]  <- round(apply(evRef[id,-1],2,sum)/30,0)
      # relative change
      tableSce[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
      tableSce[i,-1]  <- round(apply(evSce[id,-1],2,sum)/30,0)
      reltable[,-1]    <- round( (100 * (tableSce[,-1] - tableRef[,-1]) / tableRef[,-1]),2)

    }
  } # end seasonal variables
  reltable[,1] <- c("year","winter","spring","summer","autumn")

  write.table(format(reltable,width=8,nsmall=2), ofile,col.names=F,row.names=F,quote=F)
  return(reltable)
}
