% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tgIndices.R
\name{TempAvgIndices}
\alias{TempAvgIndices}
\title{Calculates a set of TG related indices}
\usage{
TempAvgIndices(input, index, scenario, horizon = 2030, season,
  regions = "NLD", ofile = NA)
}
\arguments{
\item{input}{Name of the input file (ASCII) that contains reference data
(all numerics) in which the columns provide time series for
specific stations.
The first column should provide either 00000000 or a
datestring YYYYMMDD:
Rows starting with 00000000 are considered station info
(station number, lat, lon etc.) and are ignored.
Rows starting with a datestring refer to a specific day in the
time series.
Rows starting with "#" are completely ignored and returned
unchanged.}

\item{index}{vector of indices ("aTG", "amnTG", "amxTG")}

\item{scenario}{scenario ("GL", "GH", "WL", "WH"). If scenario is not one of the 4
then the indices are calculated for the reference period 1981-2010}

\item{horizon}{time horizon ( DEFAULT=2030, 2050, 2085). If horizon is not one of the 3
then the indices are calculated for the reference period 1981-2010}

\item{season}{season (0= year, 1=winter, 2=spring, 3=summer, 4=autumn)}

\item{regions}{vector of regions
KNMI14 distinguishes following regions:\cr
<NLD> Nederland (DEFAULT) \cr
<NWN> Noordwest Nederland \cr
<ZWN> Zuidwest Nederland \cr
<NON> Noordoost Nederland \cr
<MON> Middenoost Nederland \cr
         <ZON> Zuidoost Nederland}

\item{ofile}{(DEFAULT=NA) Name of the output file to write the indices to.
Format is similar to input without the 5 first lines}
}
\description{
Calculates a set of daily average temperature related indices as
they were defined for the KNMI14 scenarios brochure
}
