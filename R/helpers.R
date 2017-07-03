SeasonalSplit <- function(season, date) {
  #Seasons
  mm <- (date %/% 100) %% 100
  ss <- (mm / 3) %% 4 + 1
  ss <- as.integer(ss)
  yy <- date %/% 10000
  wy <- ifelse(mm < 12, yy, yy + 1)

  switch(season,
         "year" = id <- 1:length(yy),
         "winter" = id <- which(ss == 1 & wy > min(wy) & wy < max(wy)),
         "spring" = id <- which(ss == 2),
         "summer" = id <- which(ss == 3),
         "autumn" = id <- which(ss == 4))
  idy <- yy[id]
  list(id = id, idy = idy)
}

MakeCombinations <- function(indices) {
  seasons <- c("year", "winter", "spring", "summer", "autumn")
  combinations <- expand.grid(index = indices,
                              season = seasons,
                              scenario = "ref",
                              horizon = 1981,
                              stringsAsFactors = FALSE)

  combinations <- rbind(combinations,
                        expand.grid(index = indices,
                                    season = seasons,
                                    scenario = "GL",
                                    horizon = 2030,
                                    stringsAsFactors = FALSE))

  combinations <- rbind(combinations,
                        expand.grid(index = indices,
                                    season = seasons,
                                    scenario = c("GL", "GH", "WL", "WH"),
                                    horizon = c(2050, 2085),
                                    stringsAsFactors = FALSE))
  combinations
}
