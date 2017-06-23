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
#' @param season     season (0= year, 1=winter, 2=spring, 3=summer, 4=autumn)
#' @param subscenario subscenario for extreme precipitation
#' ("lower", "centr" (=DEFAULT), "upper")
#' @param ofile      (DEFAULT=NA) Name of the output file to write the indices to.
#'                   Format is similar to input without the 5 first lines
#' @export
PrecipThreshIndices<- function(input, threshold, scenario = NA,
                          horizon = NA, season, subscenario, ofile = NA) {

  StationSub <- as.character(fread(system.file("refdata","P102.txt",
      package = "scenarioIndices"))$V1)

  # calcualte index for reference; else...
  if (!scenario %in% c("GL","GH","WL","WH") && horizon !=c(2030,2050,2085)){
    input <-  knmitransformer::ReadInput("rr",
        system.file("refdata",
                    "KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt",
                    package="knmitransformer"))$obs
    dt <- input[-c(1:5),1]

    input <- input[,StationSub]

  } else {

    input <- TransformPrecip(input = input, ofile=NA, scenario=scenario,
                           horizon=horizon, subscenario = subscenario)
    stationID    <- input[(1)]
    names(input) <- as.character(stationID)

    dt <- as.vector(input[-c(1:5),1, with = FALSE])
    input  <- input[-c(1:5),StationSub, with=FALSE]
  }

  tabel <- as.data.frame(matrix(NA,5,ncol(input)))
  names(tabel) <- names(input)

  #Seasons
  mm <- (dt%/%100)%%100
  ss <- as.integer( (mm/3)%%4+1)
  yy <-  dt%/%10000
  wy <- ifelse(mm<12,yy,yy+1)

  if (season=="year"){
    id  <- 1:length(yy)
    idy <- yy
  } else {
    if (season=="winter"){
      id  <- which(ss==1 & wy > min(wy) & wy < max(wy))
      idy <- wy[id]
    } else {
      id  <- which(ss==season)
      idy <- yy[id]
    }
  }

      tabel[i,-1] <- apply(input[idy,-1]>=threshold,2,sum) / length(unique(yy))

}
