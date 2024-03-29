#' Discharge Plot of the entire Time Series
#'
#'
#'
#'
#' @description Desciptive statistics. Time series of discharge at specific station. Including all discharge measurements.
#'
#' @param data list; Contains all stations that the discharge analysis should consider. List can be created by \link[lfanalyse]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param pettitt logical; (default=F); Pettitt-Test. If TRUE : non-parametric test applied on discharge measurements. Tests for a "shift in the central tendency of a time series" (source:\link[trend]{pettitt.test} ).
#'
#' @return Graphic ; Function visualizes discharge time series.
#' @export
#' @import ggplot2
#' @import trend
#' @examples
#' \dontrun{ Qplot(mosel, "COCHEM", T)}
#'
#'

Qplot=function(data,  station, pettitt=F ){

  nbr=which(names(data)==station)
  minDate= data[[nbr]][1,1]
  l=length(data[[nbr]][,1])
  maxDate=data[[nbr]][l,1]


  min=as.numeric(substr(minDate, 1, 4))
  max=as.numeric(substr(maxDate, 1, 4))
  y=data[[nbr]]$Value
  x=seq(from=min, to= max, length=l)



  if (pettitt==F){

    titl=paste("Discharge Time Series at: ", station)

    subtitl=paste("from", format(data[[station]][,1], "%Y"),"to", format(data[[station]][(nrow(data[[station]])),1] , "%Y") )


    plot= ggplot()+geom_col(data[[nbr]], mapping=aes(x=YYYY.MM.DD,y=Value, group=1), col="darkblue")+scale_x_date(name="Year")+
      labs(title=titl, subtitle=subtitl,   y=expression('Discharge Value [m'^3*'/s]'))+theme(legend.position="none")


    return(plot)
  }
  else

    s.res=pettitt.test(data[[nbr]][,2])


  n <- s.res$nobs
  n
  i <- s.res$estimate
  datan=data[[nbr]]
  s.1 <- mean(datan[1:i,2])
  s.2 <- mean(datan[(i+1):n,2])
  s <- ts(c(rep(s.1,i), rep(s.2,(n-i))))
  s
  tsp(s) <- tsp(datan[,2])

  titl=paste("Discharge Time Series at: ", station)

  subtitl=paste("from", format(data[[station]][,1], "%Y"),"to", format(data[[station]][(nrow(data[[station]])),1] , "%Y") )
  cap=paste("With detection of shift in central tendency of time series in red (pettitt test)")

  plot=ggplot()+geom_col(aes(y=datan[,2], x=datan[,1]), col="darkblue")+geom_line(aes(y=s, x=datan[,1]), col="red")+scale_x_date(name="Year")+
    labs(title=titl, subtitle=subtitl, caption=cap,   y=expression('Discharge Value [m'^3*'/s]'))
  return(plot)



}

