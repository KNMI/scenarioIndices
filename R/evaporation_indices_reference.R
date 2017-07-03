#' Calculate potential evaporation (Makkink), year mean (seasons sum) and summer for brochure validation/ only for reference period
#' @description Function reads Makkink evaporation reference series
#' and makes season sums
#' @param ofile          (DEFAULT="uitvoer.txt") Name of the output file to write the transformed data to.
#'                Format is similar to input
#' @export
evmk_sums_reference <- function(ofile="uitvoer.txt") {

  flog.info("Running evaporation sums for reference")
  flog.debug("Version is 1.0")
  # CONSTANTS AND FUNCTIONS ###############

  evmk_ref <- fread(KnmiRefFile("KNMI14____ref_evmk___19810101-20101231_v3.2.txt"))

  ev_ref <- evmk_ref[-c(1:5)]
  ev_ref <- as.data.frame(ev_ref)
  dt <- ev_ref[,1]

  tableRef <- as.data.frame(matrix(NA,5 ,ncol(ev_ref)))
  names(tableRef) <- evmk_ref[1]

  i = 0
  # # seasonal variables
  for (season in c("year", "winter", "spring", "summer", "autumn")) {
     i = i+1
   tableRef[i,-1]  <- round(apply(ev_ref[SeasonalSplit(season,dt)$id,-1],
                                  2,sum)/30,0)
  }#end seasons
  tableRef[,1] <- c("year","winter","spring","summer","autumn")

  write.table(format(tableRef,width=8,nsmall=2), ofile,col.names=F,row.names=F,quote=F)
  return(tableRef)
}
