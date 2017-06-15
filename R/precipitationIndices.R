#' precipitation threshold index
#'
#' @description Calculates a set of daily precipitation related indices as
#' they were defined for the KNMI14 scenarios brochure
#' @param index      indices ("nID", "nWD", "nSD", "nTD", "aTX")
#' @export
PrecipThreshIndices<- function(input, threshold, scenario = NA,
                          horizon = NA, season, subscenario, ofile = NA) {


  # calcualte index for reference; else...
  if (!scenario %in% c("GL","GH","WL","WH") && horizon !=c(2030,2050,2085)){
    input <-  knmitransformer:::ReadInput("rr",
                                          system.file("refdata","KNMI14____ref_rr___19810101-20101231_v3.2.txt",
                                                      package="knmitransformer"))$obs
  } else {
    input <- TransformPrecip(input = input, ofile=NA, scenario=scenario,
                           horizon=horizon, subscenario = subscenario)
    input <- input[-(1:5), ]

  }

  input <- as.data.frame(input)

  tabel <- as.data.frame(matrix(NA,5,ncol(input)))
  names(tabel) <- names(input)

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

      tabel[i,-1] <- apply(rr[id,-1]>=threshold,2,sum) / length(unique(yy))

}



