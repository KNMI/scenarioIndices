#' Calculate potential evaporation (Makkink), year mean (seasons sum) and summer for brochure validation/ only for reference period
#' @description Function reads Makkink evaporation reference series
#' and makes season sums
#' @param ofile          (DEFAULT="uitvoer.txt") Name of the output file to write the transformed data to.
#'                Format is similar to input
#' @export
evmk_sums_reference <- function(ofile="uitvoer.txt") {

  flog.info("Running evaporation sums for reference")
  flog.debug("Version is 1.0")
  # CONSTANTS AND FUNCTIONS ####################################################


  evmk_ref <- fread(KnmiRefFile("KNMI14____ref_evmk___19810101-20101231_v3.2.txt"))


  ev_ref <- evmk_ref[-c(1:5)]
  ev_ref <- as.data.frame(ev_ref)

  mm <- (ev_ref[,1] %/%100 )%%100
  ss <- as.integer( (mm /3 )%%4+1)
  yy <-  ev_ref[,1] %/%10000
  wy <- ifelse(mm <12,yy,yy+1)

  products <- data.frame("sum"=1)
  drempels <- vector()

  table_ref <- as.data.frame(matrix(NA,5 * (length(drempels) +sum(products)),ncol(ev_ref)))
  names(table_ref) <- evmk_ref[1]

  i = 0

  # seasonal variables
  for (season in 0:4) {
    if (season == 0) {
      id  <- 1:length(yy)
    } else {
      if (season == 1) {
        id  <- which(ss == 1 & wy > min(wy) & wy < max(wy))
      } else {
        id  <- which(ss == season)
      }
    }

    if (length(drempels > 0)) {
      for (j in 1:length(drempels)) {
        i = i+1
        dname <- paste("N_",drempels[j],"mm",season,sep = "")
        table_ref[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
        table_ref[i,-1] <- apply(ev_ref[id,-1]>=drempels[j],2,sum) / length(unique(yy))
      }
    }

    prod="sum"   # sd seizoenssom EV
    if (products[prod]==1) {
      dname=paste(prod,season,sep="")
      i = i+1
      table_ref[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
      #table[i,-1] <- apply(X[,-1]       , 2          ,   sd)
      table_ref[i,-1]  <- round(apply(ev_ref[id,-1],2,sum)/30,0)
    }
  } # end seasonal variables
  table_ref[,1] <- c("year","winter","spring","summer","autumn")

  write.table(format(table_ref,width=8,nsmall=2), ofile,col.names=F,row.names=F,quote=F)
  return(table_ref)
}
