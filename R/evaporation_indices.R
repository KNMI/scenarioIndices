#' Calculate potential evaporation (Makkink), year mean (seasons sum) and summer for brochure validation
#' @description Function reads transormed mean temperature and transformed global radiation
#' and calculates the Makkink evaporation for 'future time series' that match a certain climate, makes season sums and summer values
#' @param ifile_tg input file for tg
#' @param ifile_rsds input file for rsds
#' @param ofile          (DEFAULT="uitvoer.txt") Name of the output file to write the transformed data to.
#'                Format is similar to ifile
#' @param sc             scenario                      ["GL", "GH", "WL", "WH"]
#' @param p              time horizon                  [2030 (=DEFAULT), 2050, 2085]
#' @param regio.file     this (optional) argument provides the name of an ASCII file that relates the stations to
#'                a particular region. First column is station id and second column region
#'                KNMI14 distinguishes following regions:
#'                <NLD> Nederland            [DEFAULT]
#'                <NWN> Noordwest Nederland
#'                <ZWN> Zuidwest Nederland
#'                <NON> Noordoost Nederland
#'                <MON> Middenoost Nederland
#'                <ZON> Zuidoost Nederland
#' @export
evmk_sums_relchange<- function(ifile_tg, ifile_rsds,
                                      ofile="uitvoer.txt",
                                      sc,
                                      p=NA, regio.file = NA) {
  flog.info("Running evaporation calculation")
  flog.debug("Version is 1.0")
  # CONSTANTS AND FUNCTIONS ###############################################################################
  version="v1.0"

  if (!p %in% c(2030, 2050, 2085)) {
    flog.error("p={%s} has to be a valid period", paste(p))
  stop("Period must be valid, i.e. 2030, 2050, or 2085")
  }

  evmk_ref <- fread(system.file("refdata","KNMI14____ref_evmk___19810101-20101231_v3.2.txt", package="knmitransformer"))

  evmk_scenario <- knmitransformer::droogte_berekening_KNMI14(ifile_tg, ifile_rsds,
                                          ofile="uitvoer.txt",
                                          sc,p, regio.file)

  if (!all(evmk_ref [1:5] == evmk_scenario[1:5])) {
    flog.error("Same stations should be used for reference and scenarios")
    stop("Same stations should be used for reference and scenarios")
  }

  ev_ref <- evmk_ref[-(1:5)]
  ev_ref <- as.data.frame(ev_ref)

  ev_sce <- evmk_scenario[-(1:5)]
  ev_sce <- as.data.frame(ev_sce)

  mm <- (ev_ref[,1]%/%100)%%100
  ss <- as.integer((mm/3)%%4+1)
  yy <-  ev_ref[,1]%/%10000
  wy <- ifelse(mm<12,yy,yy+1)

  products <- data.frame("sum"=1)
  drempels <- vector()

  table_ref <- table_sce <- reltable <- as.data.frame(matrix(NA,5*(length(drempels)+sum(products)),ncol(ev_ref)))
  names(table_ref) <- evmk_ref[1]
  names(table_sce) <- evmk_ref[1]
  names(reltable) <- evmk_ref[1]


  i=0

  # seasonal variables
  for(season in 0:4) {
    if(season==0) {
      id  <- 1:length(yy)
      idy <- yy
    } else {
      if(season==1) {
        id  <- which(ss==1 & wy > min(wy) & wy < max(wy))
        idy <- wy[id]
      } else {
        id  <- which(ss==season)
        idy <- yy[id]
      }
    }

    if(length(drempels>0)) {
      for(j in 1:length(drempels)) {
        i=i+1
        dname <- paste("N_",drempels[j],"mm",season,sep="")
        table_ref[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
        table_ref[i,-1] <- apply(ev_ref[id,-1]>=drempels[j],2,sum) / length(unique(yy))
        ##scenarios
        table_sce[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
        table_sce[i,-1] <- apply(ev_sce[id,-1]>=drempels[j],2,sum) / length(unique(yy))
      }
    }

    prod="sum"   # sd seizoenssom EV
    if(products[prod]==1) {
      dname=paste(prod,season,sep="")
      i=i+1
      table_ref[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
      #X           <- aggregate(ev[id,-1],by=list(idy),  sum)
      #table[i,-1] <- apply(X[,-1]       , 2          ,   sd)
      table_ref[i,-1]  <- apply(ev_ref[id,-1],2,sum)/30
      # relative change
      table_sce[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
      table_sce[i,-1]  <- apply(ev_sce[id,-1],2,sum)/30
      reltable[,-1]    <- round((100 * (table_sce[,-1] - table_ref[,-1]) / table_ref[,-1]),2)

    }
  } # end seasonal variables
  reltable[,1] <- c("year","winter","spring","summer","autumn")

  result <- write.table(format(rbind(colnames(reltable),reltable),width=10,digits=1,justify="right"), ofile,col.names=F,row.names=F,quote=F)
  return(result)
}

