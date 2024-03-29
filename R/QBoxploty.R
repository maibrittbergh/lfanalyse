#' Discharge Boxplot of measured Values during specific (calendrical/hydrological) Year
#'
#' @param data  list; Contains all stations that the discharge analysis should consider. List can be created by \link[lfanalyse]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param year numeric; Year within time series of measurements.
#' @param h logical; Hydrological year. If h=TRUE (default); hydrological year November - October (given year/given year +1). If h=FALSE: calendrical year: January- December.
#'
#' @return Boxplot; Graphic of discharge measurements in a specific (calendrical/hydrological) year. Using \link[ggplot2]{geom_boxplot}.
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{ QBoxploty(data, "COCHEM", 2000, h=T)}
#'


QBoxploty=function(data,  station, year, h=T){



  nbr=which(names(data)==station)


  if (h==T){


    one=rep("0",2)
    two=rep("0", 10)

    for (i in 1:2){
      hydroyear1=c("11", "12" )
      one[i]=paste(year,"-",hydroyear1[i])
      one=sub(" - ", "-", one)

    }
    for (i in 1:10){
      hydroyear2=c("01","02", "03", "04", "05", "06", "07", "08", "09", "10")
      two[i]=paste(year+1,"-",hydroyear2[i])
      two=sub(" - ", "-", two)
    }
    year_=c(one,two)

    Nov=grep(year_[1],data[[nbr]][,1] )
    Dec=grep(year_[2],data[[nbr]][,1] )
    Jan=grep(year_[3],data[[nbr]][,1] )
    Feb=grep(year_[4],data[[nbr]][,1] )
    Mar=grep(year_[5],data[[nbr]][,1] )
    April=grep(year_[6],data[[nbr]][,1] )
    May=grep(year_[7],data[[nbr]][,1] )
    June=grep(year_[8],data[[nbr]][,1] )
    July=grep(year_[9],data[[nbr]][,1] )
    August=grep(year_[10],data[[nbr]][,1] )
    Sep=grep(year_[11],data[[nbr]][,1] )
    Oct=grep(year_[12],data[[nbr]][,1] )


    j=c(Nov,Dec,Jan, Feb,Mar, April, May, June, July, August, Sep, Oct)
    new_data=data[[nbr]][j,]
    titl=paste("Boxplot of Discharge Values at", station)
    plot=ggplot(new_data)+geom_boxplot(aes(y=new_data[,2], color="red"))+labs(title=titl, subtitle=paste("in the Hydrological Year", year, "/", year+1), y=expression('Discharge Value [m'^3*'/s]') )+theme(legend.position="none")
    return(plot)
  }else{
    year_=year
    j=grep(year_, data[[nbr]][,1])
    new_data=data[[nbr]][j,]
    titl=paste("Boxplot of Discharge Values at",station, "in", year)
    plot=ggplot(new_data)+geom_boxplot(aes(y=new_data[,2], color="red"))+labs(title=titl,subtitle=paste("in the Hydrological Year", year, "/", year+1), y=expression('Discharge Value [m'^3*'/s]'))+theme(legend.position="none")
    return(plot)
  }


}
