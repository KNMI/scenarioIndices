context("Precipitation deficit (mean & highest) calculations")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file("scenarioIndices_precipitationDeficit.log"))
library(data.table)

context("precipDefic  calc - Entire station set")

inputTemp <- KnmiRefFile("KNMI14____ref_tg___19810101-20101231_v3.2.txt")
inputRad  <- KnmiRefFile("KNMI14____ref_rsds___19810101-20101231_v3.2.txt")
inputPrec <- KnmiRefFile("KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt")
var       <- "tg"
regions   <- MatchRegionsOnStationId(ReadInput(var, inputTemp)$header[1, -1])
#
# test_that("precipDefic  reference", {
#
#   tmp <- PrecipDeficit_ref( = )
#
#   expect_equal_to_reference(tmp, "./regressionOutput/precipitationDeficit/KNMI14_ref_precipDefic.rds")
# })

test_that("2030 decadal prediction", {
  scenario = "GL"

  horizon = 2030

  tmp <- PrecipDeficit(inputTemp = inputTemp, inputRad = inputRad,
                           inputPrec = inputPrec, scenario = scenario,
                           horizon = horizon,
                           regions = regions)

  expect_equal_to_reference(tmp,
      "./regressionOutput/precipitationDeficit/KNMI14___2030_precipDefic.rds")
})

test_that("scenario WL", {
  scenario = "WL"

  horizon = 2050

  tmp <- PrecipDeficit(inputTemp = inputTemp, inputRad = inputRad,
                           inputPrec = inputPrec, scenario = scenario,
                           horizon = horizon,
                           regions = regions)

  expect_equal_to_reference(tmp,
      "./regressionOutput/precipitationDeficit/KNMI14_WL_2050_precipDefic.rds")

  p = 2085

  tmp <- PrecipDeficit(inputTemp = inputTemp, inputRad = inputRad,
                           inputPrec = inputPrec, scenario = scenario,
                           horizon = horizon,
                           regions = regions)

  expect_equal_to_reference(tmp,
      "./regressionOutput/precipitationDeficit/KNMI14_WL_2085_precipDefic.rds")
})

test_that("scenario WH", {
  scenario = "WH"

  horizon = 2050

  tmp <- PrecipDeficit(inputTemp = inputTemp, inputRad = inputRad,
                           inputPrec = inputPrec,  scenario = scenario,
                           horizon = horizon,
                           regions = regions)


  expect_equal_to_reference(tmp,
      "./regressionOutput/precipitationDeficit/KNMI14_WH_2050_precipDefic.rds")

  horizon = 2085

  tmp <- PrecipDeficit(inputTemp = inputTemp, inputRad = inputRad,
                           inputPrec = inputPrec, scenario = scenario,
                           horizon = horizon,
                           regions = regions)

  expect_equal_to_reference(tmp,
      "./regressionOutput/precipitationDeficit/KNMI14_WH_2085_precipDefic.rds")
})

test_that("scenario GH", {
  scenario = "GH"

  horizon = 2050

  tmp <- PrecipDeficit(inputTemp = inputTemp, inputRad = inputRad,
                           inputPrec = inputPrec, scenario = scenario,
                           horizon = horizon,
                           regions = regions)


  expect_equal_to_reference(tmp,
      "./regressionOutput/precipitationDeficit/KNMI14_GH_2050_precipDefic.rds")

  horizon = 2085

  tmp <- PrecipDeficit(inputTemp = inputTemp, inputRad = inputRad,
                       inputPrec = inputPrec, scenario = scenario,
                       horizon = horizon,
                       regions = regions)

  expect_equal_to_reference(tmp,
      "./regressionOutput/precipitationDeficit/KNMI14_GH_2085_precipDefic.rds")
})

test_that("scenario GL", {
  scenario = "GL"

  horizon = 2050

  tmp <- PrecipDeficit(inputTemp = inputTemp, inputRad = inputRad,
                       inputPrec = inputPrec , scenario = scenario,
                       horizon = horizon,
                       regions = regions)

  expect_equal_to_reference(tmp,
      "./regressionOutput/precipitationDeficit/KNMI14_GL_2050_precipDefic.rds")

  horizon = 2085

  tmp <- PrecipDeficit(inputTemp = inputTemp, inputRad = inputRad,
                       inputPrec = inputPrec, scenario = scenario,
                       horizon = horizon,
                       regions = regions)

  expect_equal_to_reference(tmp,
      "./regressionOutput/precipitationDeficit/KNMI14_GL_2085_precipDefic.rds")
})
