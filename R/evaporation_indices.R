#' Calculate potential evaporation (Makkink), year mean (seasons sum) and summer for brochure validation
#' @description Function reads transormed mean temperature and transformed global radiation
#' and calculates the Makkink evaporation for 'future time series' that match a certain climate, makes season sums and summer values
#' @export
evmk_sums_relchange<- function(ifile_ref, ifile_scenario,
                                      ofile="uitvoer.txt",
                                      sc,
                                      p=NA) {
  flog.info("Running evaporation calculation")
  flog.debug("Version is 1.0")
  # CONSTANTS AND FUNCTIONS ###############################################################################
  version="v1.0"

  if (!p %in% c(2030, 2050, 2085)) {
    flog.error("p={%s} has to be a valid period", paste(p))
  stop("Period must be valid, i.e. 2030, 2050, or 2085")
  }

  ifile_evmk <- droogte_berekening_KNMI14(ifile_tg, ifile_rsds,
                                          ofile="uitvoer.txt",
                                          delta.file.rsds,
                                          delta.file.tg = NA)
  ev <- test_evmk[-(1:5)]
  ev <- as.data.frame(ev)

  mm <- (ev[,1]%/%100)%%100
  ss <- as.integer((mm/3)%%4+1)
  yy <-  ev[,1]%/%10000
  wy <- ifelse(mm<12,yy,yy+1)
  tabel <- reltable <- as.data.frame(matrix(NA,5*(length(drempels)+sum(products)),ncol(ev)))
  names(tabel) <- test_evmk[1]
  names(reltable) <- test_evmk[1]

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
        tabel[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
        tabel[i,-1] <- apply(ev[id,-1]>=drempels[j],2,sum) / length(unique(yy))
      }
    }

    prod="sum"   # sd seizoenssom EV
    if(products[prod]==1) {
      dname=paste(prod,season,sep="")
      i=i+1
      tabel[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
      #X           <- aggregate(ev[id,-1],by=list(idy),  sum)
      #tabel[i,-1] <- apply(X[,-1]       , 2          ,   sd)
      tabel[i,-1]  <- apply(ev[id,-1],2,sum)/30
      # relative change
      reltable[i,-1]    <- 100 * (fut[,-1] - obs[,-1]) / obs[,-1]

    }
  } # end seasonal variables
}

