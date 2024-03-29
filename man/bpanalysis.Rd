% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bpanalysis.R
\name{bpanalysis}
\alias{bpanalysis}
\title{Future Boxplot LowFlow Analysis}
\usage{
bpanalysis(rivernames, sel_names, mod_data, NF, FF, FFH, P)
}
\arguments{
\item{rivernames}{character; vector of three river names, as they are listed in the dataset}

\item{sel_names}{character; vector of three river names, as they should appear in the final plot.}

\item{mod_data}{list; each entry of the list represents a river/station (catchement). For every catchment 5 different models were taken to compute historical and future discharge Values.}

\item{NF}{numerical; vector containing all the years that account for the near future. The function itself operates with hydrological years.}

\item{FF}{numerical; vector containing all the years that account for the far future. The function itself operates with hydrological years.}

\item{FFH}{numerical; vector FF minus maximum year.}

\item{P}{numerical; vector containing all the years that account for the past. The function itself operates with hydrological years.}
}
\value{
plot
}
\description{
Function Analyses 3 rivers with respect to the average annual discharge value, the NM7Q-Value and the Number of Days under the Q90 threshold. The Idea of the plot is to compare modelled data of the past and the future. For the future one can distinguish NF and FF scenarios.
}
\examples{
\dontrun{bpplot=bpanalysis(rivernames=c("Moselle_3", "Neckar_3", "Main_3"), sel_names=c("Mosel", "Neckar", "Main"),  mod_data, NF=c(2040:2069),
                 FF=c(2070:2098),
                 FFH=c(2070:2097),
                P=c(1970:1999) )}
}
