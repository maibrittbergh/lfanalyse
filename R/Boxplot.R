#' Discharge Boxplot of measured Discharge Values within entire Time Series at a Station
#'
#'@description  Desciptive statistics. Boxplot of entire time series containing discharge measurements at a specific station.
#'
#' @param data  list; Contains all stations that the discharge analysis should consider. List can be created by \link[lfanalyse]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in "data".
#'
#' @return Boxplot graphic of discharge time series. Using \link[ggplot2]{geom_boxplot}
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{ QBoxplot(mosel, "COCHEM")}

QBoxplot=function(data,  station){
  titl=paste("Boxplot of Discharge Values measured at",  station )
  plot=ggplot(data[[station]])+geom_boxplot(aes(y=data[[station]][,2], color="red"))+labs( title=titl, subtitle=paste("from", format(data[[station]][,1], "%Y"),"to", format(data[[station]][(nrow(data[[station]])),1] , "%Y") ),  y=expression('Discharge Value [m'^3*'/s]'))+theme(legend.position="none")
  return(plot)
}
