context("Temperature MAX indices")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file("scenarioIndices_MAXtemperature.log"))
library(data.table)

context("Temp tx ref indices - Entire station set")

input   <- KnmiRefFile("KNMI14____ref_tx___19810101-20101231_v3.2.txt")
ofile   <- "tmp.txt" # output file - used only temporary
var     <- "tx"
regions <- MatchRegionsOnStationId(ReadInput(var, input)$header[1, -1])

test_that("full table", {

  tmp <- TempMaxIndicesWrapper(input, regions = regions)
  expect_equal_to_reference(tmp,
                            "regressionOutput/temperature/tx_fullTable.rds")
})

test_that("reference", {
  index = "nID"

  scenario = "ref"

  horizon = 1981

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___ref_tx_nID.rds")

})

test_that("tx indices 2030 decadal prediction", {
  index = "nID"

  scenario = "GL"

  horizon = 2030

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tx_nID.rds")

  index = "nWD"
  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tx_nWD.rds")

  index = "nSD"
  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario, season = "year",
                        horizon = horizon,
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tx_nSD.rds")

  index = "nTD"
  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario, season = "year",
                        horizon = horizon,
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tx_nTD.rds")

  index = "aTX"
  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario, season = "year",
                        horizon = horizon,
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14___2030_tx_aTX.rds")

})

test_that("nID decadal prediction", {
  index = "nID"

  scenario = "GL"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile, season = "year",
                        scenario = scenario,
                        horizon = horizon,
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tx_nID.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tx_nID.rds")

  scenario = "WL"

  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tx_nID.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tx_nID.rds")

  scenario = "GH"

  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tx_nID.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tx_nID.rds")

  scenario = "WH"

  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tx_nID.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tx_nID.rds")

})

test_that("nWd decadal prediction", {

  index = "nWD"

  scenario = "GL"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tx_nWD.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tx_nWD.rds")

  scenario = "WL"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tx_nWD.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tx_nWD.rds")


  scenario = "GH"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tx_nWD.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tx_nWD.rds")

  scenario = "WH"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tx_nWD.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tx_nWD.rds")
})

test_that("nSD decadal prediction", {

   index = "nSD"

  scenario = "GL"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tx_nSD.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tx_nSD.rds")

  scenario = "WL"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tx_nSD.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tx_nSD.rds")


  scenario = "GH"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tx_nSD.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tx_nSD.rds")

  scenario = "WH"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tx_nSD.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tx_nSD.rds")
})

test_that("nTD decadal prediction", {

  index = "nTD"

  scenario = "GL"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tx_nTD.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tx_nTD.rds")

  scenario = "WL"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tx_nTD.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tx_nTD.rds")


  scenario = "GH"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tx_nTD.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tx_nTD.rds")

  scenario = "WH"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tx_nTD.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tx_nTD.rds")
})

test_that("aTX decadal prediction", {

  index = "aTX"

  scenario = "GL"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2050_tx_aTX.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GL_2085_tx_aTX.rds")

  scenario = "WL"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2050_tx_aTX.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WL_2085_tx_aTX.rds")


  scenario = "GH"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2050_tx_aTX.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_GH_2085_tx_aTX.rds")

  scenario = "WH"
  horizon = 2050

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2050_tx_aTX.rds")

  horizon = 2085

  tmp <- TempMaxIndices(input = input, index=index,
                        ofile= ofile,
                        scenario = scenario,
                        horizon = horizon, season = "year",
                        regions = regions)

  expect_equal_to_reference(tmp, "regressionOutput/temperature/KNMI14_WH_2085_tx_aTX.rds")
})
