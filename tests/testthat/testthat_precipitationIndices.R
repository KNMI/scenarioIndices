context("Precipitation Indices calculations")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file("scenarioIndices_precipitationIndices.log"))
library(data.table)

context("precipIndices  calc - Entire station set")

inputPrec   <- system.file("refdata","KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt", package = "scenarioIndices")
ofile      <- "tmp.txt" # output file - used only temporary


#
 test_that("precipIndices  reference", {
   scenario = "ref"

   horizon = 1981

   tmp <- PrecipThreshIndices(input = inputPrec, threshold = 30.0, scenario = scenario,
                              horizon = horizon, season = "year", ofile = ofile)

   expect_equal_to_reference(tmp, "./regressionOutput/precipitation/KNMI14_ref_precipitationIndices.rds")
 })
