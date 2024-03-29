% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MQ_trend.R
\name{MQ_trend}
\alias{MQ_trend}
\title{MQ_trend}
\usage{
MQ_trend(data, station, seasonal = "Y", graphic = T)
}
\arguments{
\item{data}{list; Contains all stations that the discharge analysis should consider. List can be created by \link[lfanalyse]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.}

\item{station}{character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.}

\item{seasonal}{character; Possible Inputs:  "Y"( MQ Trend of (hydrological) Years); "WI"(MQ Trend of Winters during years); "SP"(MQ Trend of Springs during (hydrological) years); "SU"(MQ Trend of Summers during (hydrological) years); "AU"(MQ Trend of Autums during (hydrological) years)}

\item{graphic}{logical; default=T. For graphic=T, function returns a graph and visualizes the trend.   For graphic=F, function returns the model coefficients as list. For the Sens Sloap Approach (using: \link[zyp]{zyp.trend.vector}) and for the linear model approach (using: \link[stats]{lm}).}
}
\value{
Graphic/ list:
\describe{
  \item{intercept_zyp}{intercept created by \link[zyp]{zyp.trend.vector}}
  \item{slope_zyp}{slope created by \link[zyp]{zyp.trend.vector}}
  \item{sig_zyp}{significance (Kendall's P-Value) for the final detrended time-series}
  \item{intercept_ls}{intercept created by \link[stats]{lm}}
  \item{slope_ls}{slope created by \link[stats]{lm}}
}
}
\description{
Calculating Trend for annualy calculated mean values. It is possible to calculate the mean values for a season or for the whole hydrological year. The trend is calculated using the Sen's Slope Estimator and linear regression.
}
\examples{
 \dontrun{
summerplot=MQ_trend(data=mata, "COCHEM", seasonal="SU", graphic=T)}
}
