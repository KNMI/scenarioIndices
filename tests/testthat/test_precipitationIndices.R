context("Precipitation Indices calculations")

library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file("scenarioIndices_precipitationIndices.log"))
library(data.table)

context("precipIndices  calc - Entire station set")

input <- KnmiRefFile("KNMI14____ref_rrcentr___19810101-20101231_v3.2.txt")
ofile     <- "tmp.txt" # output file - used only temporary
input <- ReadInput("rr", input)



test_that("indices reference", {

   tmp <- PrecIndicesWrapper2(input$obs)
   expect_equal_to_reference(tmp,
                             "regressionOutput/precipitation/precipInidces_ref.rds")
})



test_that("indices 2030 centr", {
  scenario = "GL"
  horizon = 2030
  subscenario = "centr"

  trans <- TransformPrecip(input = input, scenario=scenario,
                           horizon=horizon, subscenario = subscenario)

  setnames(trans, c("date", paste(trans[1, -1, with = FALSE])))

  trans <- trans[-c(1:5), ]
  tmp <- PrecIndicesWrapper2(input = trans)
  expect_equal_to_reference(tmp,
                            "regressionOutput/precipitation/precipInidces___2030_centr.rds")

})

test_that("indices GL 2050 centr", {
  scenario = "GL"
  horizon = 2050
  subscenario = "centr"

  trans <- TransformPrecip(input = input, scenario=scenario,
                           horizon=horizon, subscenario = subscenario)

  setnames(trans, c("date", paste(trans[1, -1, with = FALSE])))

  trans <- trans[-c(1:5), ]
  tmp <- PrecIndicesWrapper2(input = trans)
  expect_equal_to_reference(tmp,
                            "regressionOutput/precipitation/precipInidces_GL_2050_centr.rds")

})


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
