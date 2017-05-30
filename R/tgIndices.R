#' Calculates a set of daily average temperature related indices as they were defined
#' for the KNMI14 scenarios brochure
#' @description      function calculates a set of TG related incides
#' @param index      indices ("aTG", "amnTG", "amxTG")
#' @param ifile      Name of the input file (ASCII) that contains reference data
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
#' @param ofile      (DEFAULT=NA) Name of the output file to write the indices to.
#'                   Format is similar to ifile without the 5 first lines
#' @param scenario   scenario ("GL", "GH", "WL", "WH"). If scenario is not one of the 4
#'                   then the indices are calculated for the reference period 1981-2010
#' @param horizon    time horizon ( DEFAULT=2030, 2050, 2085). If horizon is not one of the 3
#'                   then the indices are calculated for the reference period 1981-2010
#' @param season     season (0= year, 1=winter, 2=spring, 3=summer, 4=autumn)
#' @param regio.file this (optional) argument provides the name of an ASCII file that relates the stations to
#'                   a particular region. First column is station id and second column region
#'                   KNMI14 distinguishes following regions:
#'                   <NLD> Nederland            [DEFAULT]
#'                   <NWN> Noordwest Nederland
#'                   <ZWN> Zuidwest Nederland
#'                   <NON> Noordoost Nederland
#'                   <MON> Middenoost Nederland
#'                   <ZON> Zuidoost Nederland
#'
#'
#' @export
TempAvgIndices<- function(ifile_tg, index,
                          ofile = NA, scenario,
                          horizon=2030, season,
                          regio.file = NA) {

#
  if (!index %in% c("aTG", "amnTG", "amxTG")) {
    stop("index should be one of aTG, amnTG, amxTG")
  }

# calcualte index for reference; else...
 if (!scenario %in% c("GL","GH","WL","WH") && horizon !=c(2030,2050,2085)){
      input <-  knmitransformer:::ReadInput("tg",
                system.file("refdata","KNMI14____ref_tg___19810101-20101231_v3.2.txt",
                            package="knmitransformer"))$obs
    } else {
        input <- TransformTemp(ifile=ifile_tg, ofile=NA, scenario=scenario,
                                                horizon=horizon, var="tg", regio.file=regio.file)
        input <- input[-(1:5) ]

    }


#
  input <- as.data.frame(input)

    #Seasons
   mm <- (input[,1]%/%100)%%100
   ss <- as.integer((mm/3)%%4+1)
   yy <-  input[,1]%/%10000
   wy <- ifelse(mm<12,yy,yy+1)

   if(season=="year"){
     id  <- 1:length(yy)
     idy <- yy
   } else {
    if(season=="winter"){
      id  <- which(ss==1 & wy > min(wy) & wy < max(wy))
      idy <- wy[id]
    } else {
      id  <- which(ss==season)
      idy <- yy[id]
    }
   }

  #Indices
     if(index=="aTG") {
       X     <- aggregate(input[id,-1],by=list(idy),  mean)
     } else {
   if(index=="amnTG") {
         X     <- aggregate(input[id,-1],by=list(idy),  min)
      } else {
    if(index=="amxTG") {
         X     <- aggregate(input[id,-1],by=list(idy),  max)
       }
     }
  }
   return(X)
}




