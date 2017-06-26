context("Precipitation Indices calculations")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file("scenarioIndices_precipitationIndices.log"))
library(data.table)

context("precipIndices  calc - Entire station set")

input <- KnmiRefFile("KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt")
subscenario <- "centr"
ofile     <- "tmp.txt" # output file - used only temporary

#
# test_that("full table centr", {
#
#   # tmp <- PrecIndicesWrapper(input, subscenario = "centr")
#   # expect_equal_to_reference(tmp,
#   #                           "regressionOutput/precipitation/pre_fullTable_centr.rds")
# })



# #
#  test_that("precipIndices  reference", {
#
#    scenario = "ref"
#
#    horizon = 1981
#
#    tmp <- PrecipThreshIndices(input = input, index = "N30mm",
#                               scenario = scenario, horizon = horizon,
#                               season = "year", subscenario = subscenario)
#
#    expect_equal_to_reference(tmp,
#       "./regressionOutput/precipitation/KNMI14_ref_precipitationIndices.rds")
#  })
