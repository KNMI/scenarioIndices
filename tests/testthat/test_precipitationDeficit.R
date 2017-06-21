context("Precipitation deficit (mean & highest) calculations")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('scenarioIndices_precipitationDeficit.log'))
library(data.table)

context("precipDefic  calc - Entire station set")

input_tg   <- system.file("refdata","KNMI14____ref_tg___19810101-20101231_v3.2.txt", package="knmitransformer")
input_rsds <- system.file("refdata","KNMI14____ref_rsds___19810101-20101231_v3.2.txt", package="knmitransformer")
input_rr   <- system.file("refdata","KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt", package = "scenarioIndices")
ofile      <- "tmp.txt" # output file - used only temporary
var <- "tg"
regions    <- knmitransformer::MatchRegionsOnStationId(knmitransformer::ReadInput(var, input_tg)$header[1, -1])
#
# test_that("precipDefic  reference", {
#
#   tmp <- PrecipDeficit_ref(ofile = ofile)
#
#   expect_equal_to_reference(tmp, "./regressionOutput/precipitationDeficit/KNMI14_ref_precipDefic.rds")
# })

test_that("2030 decadal prediction", {
  scenario = "GL"

  horizon = 2030

  tmp <- PrecipDeficit_sce(input_tg = input_tg, input_rsds = input_rsds,
                           input_rr = input_rr,ofile = ofile, scenario = scenario,
                           horizon = horizon,
                           regions = regions)

  expect_equal_to_reference(tmp, "./regressionOutput/precipitationDeficit/KNMI14___2030_precipDefic.rds")
})

test_that("scenario WL", {
  scenario = "WL"

  horizon = 2050

  tmp <- PrecipDeficit_sce(input_tg = input_tg, input_rsds = input_rsds,
                           input_rr = input_rr,ofile = ofile, scenario = scenario,
                           horizon = horizon,
                           regions = regions)

  expect_equal_to_reference(tmp, "./regressionOutput/precipitationDeficit/KNMI14_WL_2050_precipDefic.rds")

  p = 2085

  tmp <- PrecipDeficit_sce(input_tg = input_tg, input_rsds = input_rsds,
                           input_rr = input_rr,ofile = ofile, scenario = scenario,
                           horizon = horizon,
                           regions = regions)

  expect_equal_to_reference(tmp, "./regressionOutput/precipitationDeficit/KNMI14_WL_2085_precipDefic.rds")
})

test_that("scenario WH", {
  scenario = "WH"

  horizon = 2050

  tmp <- PrecipDeficit_sce(input_tg = input_tg, input_rsds = input_rsds,
                           input_rr = input_rr,ofile = ofile, scenario = scenario,
                           horizon = horizon,
                           regions = regions)


  expect_equal_to_reference(tmp, "./regressionOutput/precipitationDeficit/KNMI14_WH_2050_precipDefic.rds")

  horizon = 2085

  tmp <- PrecipDeficit_sce(input_tg = input_tg, input_rsds = input_rsds,
                           input_rr = input_rr,ofile = ofile, scenario = scenario,
                           horizon = horizon,
                           regions = regions)

  expect_equal_to_reference(tmp, "./regressionOutput/precipitationDeficit/KNMI14_WH_2085_precipDefic.rds")
})

test_that("scenario GH", {
  scenario = "GH"

  horizon = 2050

  tmp <- PrecipDeficit_sce(input_tg = input_tg, input_rsds = input_rsds,
                           input_rr = input_rr,ofile = ofile, scenario = scenario,
                           horizon = horizon,
                           regions = regions)


  expect_equal_to_reference(tmp, "./regressionOutput/precipitationDeficit/KNMI14_GH_2050_precipDefic.rds")

  horizon = 2085

  tmp <- PrecipDeficit_sce(input_tg = input_tg, input_rsds = input_rsds,
                           input_rr = input_rr,ofile = ofile, scenario = scenario,
                           horizon = horizon,
                           regions = regions)

  expect_equal_to_reference(tmp, "./regressionOutput/precipitationDeficit/KNMI14_GH_2085_precipDefic.rds")
})

test_that("scenario GL", {
  scenario = "GL"

  horizon = 2050

  tmp <- PrecipDeficit_sce(input_tg = input_tg, input_rsds = input_rsds,
                           input_rr = input_rr,ofile = ofile, scenario = scenario,
                           horizon = horizon,
                           regions = regions)

  expect_equal_to_reference(tmp, "./regressionOutput/precipitationDeficit/KNMI14_GL_2050_precipDefic.rds")

  horizon = 2085

  tmp <- PrecipDeficit_sce(input_tg = input_tg, input_rsds = input_rsds,
                           input_rr = input_rr,ofile = ofile, scenario = scenario,
                           horizon = horizon,
                           regions = regions)

  expect_equal_to_reference(tmp, "./regressionOutput/precipitationDeficit/KNMI14_GL_2085_precipDefic.rds")
})
