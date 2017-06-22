#' Calculates a set of TG related indices
#'
#' @description Calculates a set of daily average temperature related indices as
#' they were defined for the KNMI14 scenarios brochure
#' @param index      vector of indices ("aTG", "amnTG", "amxTG")
#' @param input      Name of the input file (ASCII) that contains reference data
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
#'                   Format is similar to input without the 5 first lines
#' @param scenario   scenario ("GL", "GH", "WL", "WH"). If scenario is not one of the 4
#'                   then the indices are calculated for the reference period 1981-2010
#' @param horizon    time horizon ( DEFAULT=2030, 2050, 2085). If horizon is not one of the 3
#'                   then the indices are calculated for the reference period 1981-2010
#' @param season     season (0= year, 1=winter, 2=spring, 3=summer, 4=autumn)
#' @param regions     vector of regions
#'                   KNMI14 distinguishes following regions:\cr
#'                   <NLD> Nederland (DEFAULT) \cr
#'                   <NWN> Noordwest Nederland \cr
#'                   <ZWN> Zuidwest Nederland \cr
#'                   <NON> Noordoost Nederland \cr
#'                   <MON> Middenoost Nederland \cr
#'                            <ZON> Zuidoost Nederland
#'
#'
#' @export
TempAvgIndices<- function(input, index, scenario,
                          horizon = 2030, season,
                          regions = "NLD", ofile = NA) {

#
  if (!index %in% c("aTG", "amnTG", "amxTG")) {
    stop("index should be one of aTG, amnTG, amxTG")
  }

# calcualte index for reference; else...
 if (!scenario %in% c("GL","GH","WL","WH") && horizon !=c(2030,2050,2085)){
      input <-  knmitransformer::ReadInput("tg",
                system.file("refdata","KNMI14____ref_tg___19810101-20101231_v3.2.txt",
                            package="knmitransformer"))$obs
    } else {
        input <- TransformTemp(input=input, ofile=NA, scenario=scenario,
                                                horizon=horizon, var="tg",regions = regions)
        input <- input[-(1:5), ]

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
   switch(index,
          "aTG" = X <- aggregate(input[id,-1],by=list(idy),  mean),
          "amnTG" = X <- aggregate(input[id,-1],by=list(idy),  min),
          "amxTG" = X <- aggregate(input[id,-1],by=list(idy),  max))

   return(X)
}




