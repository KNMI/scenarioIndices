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

   tmp <- PrecipThreshIndices(input = inputPrec, threshold, scenario = scenario,
                              horizon = horizon, ofile = ofile)

   expect_equal_to_reference(tmp, "./regressionOutput/precipitation/KNMI14_ref_precipitationIndices.rds")
 })
