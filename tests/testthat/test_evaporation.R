context("Evaporation sums calculations")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('scenarioIndices_evaporation.log'))
library(data.table)

context("evmk sum calc - Entire station set")

input_tg   <- system.file("refdata","KNMI14____ref_tg___19810101-20101231_v3.2.txt", package="knmitransformer")
input_rsds <- system.file("refdata","KNMI14____ref_rsds___19810101-20101231_v3.2.txt", package="knmitransformer")
ofile      <- "tmp.txt" # output file - used only temporary
regio.file <- "stationstabel"


test_that("evaporation sums reference", {

  tmp <- evmk_sums_reference(ofile = ofile)

  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_ref_evmk_sums.rds")
})

test_that("2030 decadal prediction", {
  scenario = "GL"

  horizon = 2030

  tmp <- evmk_sums_relchange(input_tg=input_tg, input_rsds=input_rsds,
                             ofile = ofile,
                             scenario = scenario,
                             horizon = horizon,
                             regio.file = regio.file)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14___2030_evmk_sums.rds")
})

test_that("scenarioenario WL", {
  scenario = "WL"

  horizon = 2050
  tmp <- evmk_sums_relchange(input_tg = input_tg, input_rsds = input_rsds,
                             ofile = ofile,
                             scenario = scenario,
                             horizon = horizon,
                             regio.file = regio.file)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WL_2050_evmk_sums.rds")

  horizon = 2085
  tmp <- evmk_sums_relchange(input_tg=input_tg, input_rsds=input_rsds,
                             ofile=ofile,
                             scenario=scenario,
                             horizon = horizon,
                             regio.file = regio.file)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WL_2085_evmk_sums.rds")
})

test_that("scenarioenario WH", {
  scenario = "WH"

  horizon = 2050
  tmp <- evmk_sums_relchange(input_tg=input_tg, input_rsds=input_rsds,
                             ofile=ofile,
                             scenario=scenario,
                             horizon = horizon,
                             regio.file = regio.file)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WH_2050_evmk_sums.rds")

  horizon = 2085
  tmp <- evmk_sums_relchange(input_tg=input_tg, input_rsds=input_rsds,
                             ofile=ofile,
                             scenario=scenario,
                             horizon = horizon,
                             regio.file = regio.file)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WH_2085_evmk_sums.rds")
})

test_that("scenarioenario GH", {
  scenario = "GH"

  horizon = 2050
  tmp <- evmk_sums_relchange(input_tg=input_tg, input_rsds=input_rsds,
                             ofile=ofile,
                             scenario=scenario,
                             horizon = horizon,
                             regio.file = regio.file)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GH_2050_evmk_sums.rds")

  horizon = 2085
  tmp <- evmk_sums_relchange(input_tg=input_tg, input_rsds=input_rsds,
                             ofile=ofile,
                             scenario=scenario,
                             horizon = horizon,
                             regio.file = regio.file)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GH_2085_evmk_sums.rds")
})

test_that("scenarioenario GL", {
  scenario = "GL"

  horizon = 2050
  tmp <- evmk_sums_relchange(input_tg=input_tg, input_rsds=input_rsds,
                             ofile=ofile,
                             scenario=scenario,
                             horizon = horizon,
                             regio.file = regio.file)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GL_2050_evmk_sums.rds")

  horizon = 2085
  tmp <- evmk_sums_relchange(input_tg=input_tg, input_rsds=input_rsds,
                             ofile=ofile,
                             scenario=scenario,
                             horizon = horizon,
                             regio.file = regio.file)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GL_2085_evmk_sums.rds")
})
