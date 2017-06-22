#' precipitation threshold index
#'
#' @description Calculates a set of daily precipitation related indices as
#' they were defined for the KNMI14 scenarios brochure
#' @param input Name of the input file (ASCII) that contains reference data
#'                   (all numerics) in which the columns provide time series for
#'                   specific stations.
#'                   The first column should provide either 00000000 or a
#'                   datestring YYYYMMDD:
#'                   Rows starting with 00000000 are considered station info
#'                   (station number, lat, lon etc.) and are ignored.
#'                   Rows starting with a datestring refer to a specific day in the
#'                   time series.
#'                   Rows starting with "#" are completely ignored and returned
#'                   unchanged.
#' @param threshold vector of thresholds
#' @param scenario   scenario ("GL", "GH", "WL", "WH"). If scenario is not one of the 4
#'                   then the indices are calculated for the reference period 1981-2010
#' @param horizon    time horizon ( DEFAULT=2030, 2050, 2085). If horizon is not one of the 3
#'                   then the indices are calculated for the reference period 1981-2010
#' @param subscenario subscenario for extreme precipitation
#' ("lower", "centr" (=DEFAULT), "upper")
#' @param ofile      (DEFAULT=NA) Name of the output file to write the indices to.
#'                   Format is similar to input without the 5 first lines
#' @export
PrecipThreshIndices<- function(input, threshold, scenario = NA,
                          horizon = NA, subscenario, ofile = NA) {


  StationSub <- as.character(fread(system.file("refdata","P102.txt", package = "scenarioIndices"))$V1)

  # calcualte index for reference; else...
  # if (!scenario %in% c("GL","GH","WL","WH") && horizon !=c(2030,2050,2085)){
  #   input <-  knmitransformer:::ReadInput("rr",
  #                                         system.file("refdata","KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt",
  #                                                     package="knmitransformer"))$obs
  #   input <- input[,StationSub]
  # } else {
    input <- knmitransformer::TransformPrecip(input = input, ofile=NA, scenario=scenario,
                           horizon=horizon, subscenario = subscenario)
   stationID         <- input[(1)]
    names(input) <- as.character(stationID)
    input <- input[-(1:5),StationSub, with=FALSE]

  # }

  input <- as.data.frame(input)

  products <- data.frame("sd.Psum"=1,"Psum"=1)

  threshold <- c(0.1,0.3,1.0,5.0,10.0,15.0,20.0,30.0)

  tabel <- as.data.frame(matrix(NA,5*(length(threshold)+sum(products)),ncol(input)))
  names(tabel) <- names(input)


  #Seasons
  mm <- (input[,1]%/%100)%%100
  ss <- as.integer((mm/3)%%4+1)
  yy <-  input[,1]%/%10000
  wy <- ifelse(mm<12,yy,yy+1)

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

    if(length(threshold)>0) {
      for(j in 1:length(threshold)) {
        i=i+1
        dname <- paste("N_",season, "_",threshold[j],"mm",sep="")
        tabel[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
        tabel[i,-1] <- apply(input[id,-1]>=threshold[j],2,sum) / length(unique(yy))
      }
    }

    prod="Psum"   # mean seizoenssom RR
    if(products[prod]==1) {
      dname=paste(prod,"_",season,sep="")
      i=i+1
      tabel[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
      X           <- aggregate(input[id,-1],by=list(idy),  sum)
      tabel[i,-1] <- apply(X[,-1]       , 2          , mean)
    }

    prod="sd.Psum"   # sd seizoenssom RR
    if(products[prod]==1) {
      dname=paste(prod,"_",season,sep="")
      i=i+1
      tabel[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
      X           <- aggregate(input[id,-1],by=list(idy),  sum)
      tabel[i,-1] <- apply(X[,-1]       , 2          ,   sd)
    }

  } # end seasonal variables
  return(tabel)
}



