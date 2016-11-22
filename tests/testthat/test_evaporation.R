context("Evaporation sums calculations")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('scenarioIndices_evaporation.log'))
library(data.table)

context("evmk sum calc - Entire station set")

ifile_tg   <- system.file("refdata","KNMI14____ref_tg___19810101-20101231_v3.2.txt", package="knmitransformer")
ifile_rsds <- system.file("refdata","KNMI14____ref_rsds___19810101-20101231_v3.2.txt", package="knmitransformer")
ofile      <- "tmp.txt" # output file - used only temporary
regio.file <- system.file("extdata","stationstabel", package="knmitransformer") # table that links stations to region

test_that("2030 decadal prediction", {
  sc="GL"

  p=2030

  tmp <- evmk_sums_relchange(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                             ofile = ofile,
                             sc=sc,
                             p=p)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14___2030_evmk_sums.rds")
})

test_that("Scenario WL", {
  sc="WL"

  p=2050
  tmp <- evmk_sums_relchange(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                             ofile=ofile,
                             sc=sc,
                             p=p)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WL_2050_evmk_sums.rds")

  # Regression test based only on the smaller subset used also for the other
  # scenarios
  p = 2085
  tmp <- evmk_sums_relchange(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                             ofile=ofile,
                             sc=sc,
                             p=p)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WL_2085_evmk_sums.rds")
})

test_that("Scenario WH", {
  sc="WH"

  p=2050
  tmp <- evmk_sums_relchange(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                             ofile=ofile,
                             sc=sc,
                             p=p)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WH2050_evmk_sums.rds")

  p = 2085
  tmp <- evmk_sums_relchange(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                             ofile=ofile,
                             sc=sc,
                             p=p)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_WH_2085_evmk_sums.rds")
})

test_that("Scenario GH", {
  sc="GH"

  p=2050
  tmp <- evmk_sums_relchange(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                             ofile=ofile,
                             sc=sc,
                             p=p)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GH_2050_evmk_sums.rds")

  p = 2085
  tmp <- evmk_sums_relchange(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                             ofile=ofile,
                             sc=sc,
                             p=p)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GH_2085_evmk_sums.rds")
})

test_that("Scenario GL", {
  sc="GL"

  p=2050
  tmp <- evmk_sums_relchange(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                             ofile=ofile,
                             sc=sc,
                             p=p)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GL_2050_evmk_sums.rds")

  p = 2085
  tmp <- evmk_sums_relchange(ifile_tg=ifile_tg, ifile_rsds=ifile_rsds,
                             ofile=ofile,
                             sc=sc,
                             p=p)



  expect_equal_to_reference(tmp, "regressionOutput/evaporation/KNMI14_GL_2085_evmk_sums.rds")
})
