% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Boxplot.R
\name{QBoxplot}
\alias{QBoxplot}
\title{Discharge Boxplot of measured Discharge Values within entire Time Series at a Station}
\usage{
QBoxplot(data, station)
}
\arguments{
\item{data}{list; Contains all stations that the discharge analysis should consider. List can be created by \link[lfanalyse]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.}

\item{station}{character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in "data".}
}
\value{
Boxplot graphic of discharge time series. Using \link[ggplot2]{geom_boxplot}
}
\description{
Desciptive statistics. Boxplot of entire time series containing discharge measurements at a specific station.
}
\examples{
\dontrun{ QBoxplot(mosel, "COCHEM")}
}
