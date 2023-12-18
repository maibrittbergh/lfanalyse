#' Rasterplot of a Discharge Time Series
#'
#' @description Function displays discharge Values at a station. Rasterplot covers entire time series
#'

#' @param data list; Contains all stations that the discharge analysis should consider. List can be created by \link[lfanalyse]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the station e.g. "COCHEM" - must be named equally like list entry in data.
#'@import ggplot2
#' @return Graphic ; Function visualizes discharge time series as a rasterplot.
#' @export
#'
#' @examples
#' \dontrun{ Rasterplot("COCHEM", data )}
Rasterplot=function(station, data){


# Creating Dataset --------------------------------------------------------



  nbr=which(names(data)==station)
dataset=data[[nbr]]


    l= nrow(data[[nbr]])
    dates=rep(0,l)
      for (i in 1:  l){
        dates[i]=as.numeric(substr(data[[nbr]][i,1], 1, 4))
      }

    dataset=cbind(dataset, dates)
    month=rep(0,l)
    for (i in 1:  l){
      month[i]=as.numeric(substr(data[[nbr]][i,1], 6, 7))
    }



    dataset=cbind(dataset, month)

    month_chr=function(datan){
      l= length(datan)
    vec=rep(0,l)
    for ( i in 1:l){
      if (datan[i]==1) {
       h="Jan"
      } else if (datan[i]==2){
        h="Feb"
      } else if (datan[i]==3){
        h="Mar"
      } else if (datan[i]==4){
        h="Apr"
      } else if (datan[i]==5){
        h="May"
      } else if (datan[i]==6){
        h="Jun"
      } else if (datan[i]==7){
        h="Jul"
      }  else if (datan[i]==8){
        h="Aug"
      } else if (datan[i]==9){
        h="Sep"
      } else if (datan[i]==10){
        h="Oct"
      }  else if (datan[i]==11){
        h="Nov"
      } else if (datan[i]==12){
        h="Dec"}



      vec[i]=h
    }
    return(vec)}

      chr=month_chr(dataset$month)


dataset=cbind(dataset, chr)

# Statistics of data ------------------------------------------------------


Mean=mean(dataset$Value)


med=median(dataset$Value)



# Graphical ---------------------------------------------------------------

titl=paste("Rasterplot of Discharge Time Series at: ", station)

subtitl=paste("from", format(data[[station]][,1], "%Y"),"to", format(data[[station]][(nrow(data[[station]])),1] , "%Y") )[1]
subtitl

plot=ggplot(dataset, aes(chr, dates )) +
  geom_raster(aes(fill = Value)) + scale_fill_viridis_c(option = "D", trans = "log10")+scale_x_discrete(limit=c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))+ xlab("Month")+ylab("Year") +
  labs(fill=expression('log10(Discharge Value[m'^3*'/s])'), title = titl, subtitle=subtitl)


return(plot)
}

