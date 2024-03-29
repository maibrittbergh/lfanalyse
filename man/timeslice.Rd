% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeslice.R
\name{timeslice}
\alias{timeslice}
\title{Time Sclice to compare annual minimum discharge Values}
\usage{
timeslice(data, station, min1, max1, min2, max2)
}
\arguments{
\item{data}{list; contains all stations that the discharge analysis should consider. List can be created by \link[lfanalyse]{grdc_reader}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.}

\item{station}{character; Name of the station. Must equal one entry of the data list.}

\item{min1}{numeric; Startyear of first timeslice (e.g. 1940)}

\item{max1}{numeric; Endyear of first timeslice (e.g. 1970)}

\item{min2}{numeric; Startyear of second timeslice (e.g. 1970)}

\item{max2}{numeric; Startyear of second timeslice (e.g. 2000)}
}
\description{
Function returns two boxplots. Each boxplot shows the distribution of annual minimum discharge values for a certain time sclice. The length as well as the start- and enddate can be chosen by the function paramters. In order to achieve comparability between the timesclices it is important to choose the same length for both timeslices.
}
\examples{
\dontrun{timeslice(data, "DRESDEN", 1940, 1970, 1970, 2000)}
}
