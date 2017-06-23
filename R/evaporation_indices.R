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
evmk_sums_relchange <- function(inputTemp, inputRad, scenario,
                                      horizon = NA, regions = "NLD", ofile=NA) {

  flog.info("Running evaporation calculation")
  flog.debug("Version is 1.0")
  # CONSTANTS AND FUNCTIONS ####################################################

  if (!horizon %in% c(2030, 2050, 2085)) {
    flog.error("horizon={%s} has to be a valid period", paste(horizon))
  stop("Horizon must be valid, i.e. 2030, 2050, or 2085")
  }

  evmk_ref <- fread(system.file("refdata",
                                "KNMI14____ref_evmk___19810101-20101231_v3.2.txt",
                                package="knmitransformer"))

  evmk_scenario <- TransformEvap(inputTemp = inputTemp,
                                 inputRad = inputRad,
                                 ofile=NA,
                                 scenario = scenario,
                                 horizon = horizon,
                                 regions = regions)

  if (!all(evmk_ref [1:5] == evmk_scenario[1:5])) {
    flog.error("Same stations should be used for reference and scenarios")
    stop("Same stations should be used for reference and scenarios")
  }

  ev_ref <- evmk_ref[-c(1:5)]
  ev_ref <- as.data.frame(ev_ref)

#  ev_sce <- round(evmk_scenario[-(1:5)],1)
  ev_sce <- evmk_scenario[-c(1:5)]
    ev_sce <- as.data.frame(ev_sce)

  mm <- (ev_ref[,1]%/%100)%%100
  ss <- as.integer( (mm/3)%%4+1)
  yy <-  ev_ref[,1]%/%10000
  wy <- ifelse(mm<12,yy,yy+1)

  products <- data.frame("sum"=1)
  drempels <- vector()

  table_sce <-  table_ref <- reltable <- as.data.frame(matrix(NA,
      5 * (length(drempels)+sum(products)),ncol(ev_ref)))
  names(table_ref) <- evmk_ref[1]
  names(table_sce) <- evmk_ref[1]
  names(reltable) <- evmk_ref[1]


  i=0

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
        table_ref[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname))) # nolint
        table_ref[i,-1] <- apply(ev_ref[id,-1]>=drempels[j],2,sum) / length(unique(yy))
        ##scenarios
        table_sce[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
        table_sce[i,-1] <- apply(ev_sce[id,-1]>=drempels[j],2,sum) / length(unique(yy))
      }
    }

    prod="sum"   # sd seizoenssom EV
    if (products[prod]==1) {
      dname=paste(prod,season,sep="")
      i=i+1
      table_ref[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
      #table[i,-1] <- apply(X[,-1]       , 2          ,   sd)
      table_ref[i,-1]  <- round(apply(ev_ref[id,-1],2,sum)/30,0)
      # relative change
      table_sce[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
      table_sce[i,-1]  <- round(apply(ev_sce[id,-1],2,sum)/30,0)
      reltable[,-1]    <- round( (100 * (table_sce[,-1] - table_ref[,-1]) / table_ref[,-1]),2)

    }
  } # end seasonal variables
  reltable[,1] <- c("year","winter","spring","summer","autumn")

  write.table(format(reltable,width=8,nsmall=2), ofile,col.names=F,row.names=F,quote=F)
  return(reltable)
}
