% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seasonpl.R
\name{seasonpl}
\alias{seasonpl}
\title{Seasonal Plot}
\usage{
seasonpl(data, station, Startyear, Endyear, month_start, month_end)
}
\arguments{
\item{data}{list; River from GRDC - dataset. Output of \link[lfanalyse]{grdc_readr}. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).}

\item{station}{character; Must equal name of station in data.}

\item{Startyear}{numeric; Startyear.}

\item{Endyear}{numeric; Endyear.}

\item{month_start}{numeric; Begin of the season. E.g. Febuary=2}

\item{month_end}{numeric; End of the seasoan. E.g. August=8}
}
\value{
Seasonal plot. The x-axis is scaled by the choosen season.
}
\description{
Function returns discharge plot of a given season at a given station. It is possible to choose a timeframe of multiple years.
It allows the user to compare the same season/timespan within different hydrological years. Minimum startmonth: November. Maximum endmonth: October.
}
\examples{
\dontrun{
seasonpl(mosel, "COCHEM", 2013, 2017, 2,3)
}

}
