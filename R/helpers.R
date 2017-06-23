SeasonalSplit <- function(season, date) {
  #Seasons
  mm <- (date %/% 100) %% 100
  ss <- (mm / 3) %% 4 + 1
  ss <- as.integer(ss)
  yy <- date %/% 10000
  wy <- ifelse(mm < 12, yy, yy + 1)

  if (season == "year"){
    id  <- 1:length(yy)
    idy <- yy
  } else {
    if (season == "winter"){
      id  <- which(ss == 1 & wy > min(wy) & wy < max(wy))
      idy <- wy[id]
    } else {
      id  <- which(ss == season)
      idy <- yy[id]
    }
  }
  list(id = id, idy = idy)
}
