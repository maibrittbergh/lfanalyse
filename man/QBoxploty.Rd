% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QBoxploty.R
\name{QBoxploty}
\alias{QBoxploty}
\title{Discharge Boxplot of measured Values during specific (calendrical/hydrological) Year}
\usage{
QBoxploty(data, station, year, h = T)
}
\arguments{
\item{data}{list; Contains all stations that the discharge analysis should consider. List can be created by \link[lfanalyse]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.}

\item{station}{character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.}

\item{year}{numeric; Year within time series of measurements.}

\item{h}{logical; Hydrological year. If h=TRUE (default); hydrological year November - October (given year/given year +1). If h=FALSE: calendrical year: January- December.}
}
\value{
Boxplot; Graphic of discharge measurements in a specific (calendrical/hydrological) year. Using \link[ggplot2]{geom_boxplot}.
}
\description{
Discharge Boxplot of measured Values during specific (calendrical/hydrological) Year
}
\examples{
\dontrun{ QBoxploty(data, "COCHEM", 2000, h=T)}

}
