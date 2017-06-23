context("Temperature AVG indices")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file("scenarioIndices_AVGtemperature.log"))
library(data.table)

context("Temp tg ref indices - Entire station set")

input   <- KnmiRefFile("KNMI14____ref_tg___19810101-20101231_v3.2.txt")
ofile   <- "tmp.txt" # output file - used only temporary
var     <- "tg"
regions <- MatchRegionsOnStationId(ReadInput(var, input)$header[1, -1])

test_that("full table", {

  tmp <- TempAvgIndicesWrapper(input, regions = regions)
  expect_equal_to_reference(tmp,
      "regressionOutput/temperature/tg_fullTable.rds")
})


test_that("reference", {
  index = "aTG"

  scenario = "ref"

  horizon = 1981

  tmp <- TempAvgIndices(input= input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___ref_tg_aTG.rds")

  tmp <- TempAvgIndices(input= input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "winter",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___ref_tg_aTG_winter.rds")

  tmp <- TempAvgIndices(input= input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "spring",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___ref_tg_aTG_spring.rds")


  tmp <- TempAvgIndices(input= input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "summer",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___ref_tg_aTG_summer.rds")


  tmp <- TempAvgIndices(input= input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "autumn",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___ref_tg_aTG_autumn.rds")


})
test_that("2030 decadal prediction", {
  index = "aTG"

  scenario = "GL"

  horizon = 2030

  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tg_aTG.rds")

  index = "amnTG"
  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tg_amnTG.rds")

  index = "amxTG"
  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario, season = "year",
                        horizon = horizon,
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tg_amxTG.rds")

})

test_that("aTG decadal prediction", {
  index = "aTG"

  scenario = "GL"
  horizon = 2050

  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile, season = "year",
                        scenario = scenario,
                        horizon = horizon,
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_aTG.rds")

  horizon = 2085

  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_aTG.rds")

  scenario = "WL"

  horizon = 2050

  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_aTG.rds")

  horizon = 2085

  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_aTG.rds")

  scenario = "GH"

  horizon = 2050

  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_aTG.rds")

  horizon = 2085

  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_aTG.rds")

  scenario = "WH"

  horizon = 2050

  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_aTG.rds")

  horizon = 2085

  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_aTG.rds")

})

test_that("amnTG decadal prediction", {
  index = "amnTG"

  scenario = "GL"
  horizon = 2050

  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_amnTG.rds")

  horizon = 2085

  tmp <- TempAvgIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_amnTG.rds")

  scenario = "WL"
  horizon = 2050

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_amnTG.rds")

  horizon = 2085

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_amnTG.rds")


  scenario = "GH"
  horizon = 2050

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_amnTG.rds")

  horizon = 2085

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_amnTG.rds")

  scenario = "WH"
  horizon = 2050

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_amnTG.rds")

  horizon = 2085

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_amnTG.rds")
})

test_that("amxTG decadal prediction", {
  index = "amxTG"

  scenario = "GL"
  horizon = 2050

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_amxTG.rds")

  horizon = 2085

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_amxTG.rds")

  scenario = "WL"
  horizon = 2050

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_amxTG.rds")

  horizon = 2085

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_amxTG.rds")


  scenario = "GH"
  horizon = 2050

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_amxTG.rds")

  horizon = 2085

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_amxTG.rds")

  scenario = "WH"
  horizon = 2050

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_amxTG.rds")

  horizon = 2085

  tmp <- TempAvgIndices(input =  input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_amxTG.rds")
})
