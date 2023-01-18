#' Time Sclice to compare annual minimum discharge Values
#'
#'@description Function returns two boxplots. Each boxplot shows the distribution of annual minimum discharge values for a certain time sclice. The length as well as the start- and enddate can be chosen by the function paramters. In order to achieve comparability between the timesclices it is important to choose the same length for both timeslices.
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[lfanalyse]{grdc_reader}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the station. Must equal one entry of the data list.
#' @param min1 numeric; Startyear of first timeslice (e.g. 1940)
#' @param max1 numeric; Endyear of first timeslice (e.g. 1970)
#' @param min2 numeric; Startyear of second timeslice (e.g. 1970)
#' @param max2 numeric; Startyear of second timeslice (e.g. 2000)
#'
#' @import ggplot2
#' @export
#'
#' @examples \dontrun{timeslice(data, "DRESDEN", 1940, 1970, 1970, 2000)}
timeslice=function(data, station, min1, max1, min2, max2){


data=data[[station]]

val=data[,2]
abs_min=min(val)
#Minima der Jahre
year_one=as.numeric(substring(as.character(data[1,1]),1,4))

last_year=as.numeric(substring(as.character(data[length(val),1]),1,4))
years=c(year_one:last_year)
l=length(years)
q_min=rep(0, l)
for ( i in 1:l){
  year=as.character(years[i])
  j=grep(year, data[,1])
  Val=data[,2][j]
  q_min[i]=min(Val)
}
results=data.frame(years, q_min)

# create dataset using two campared datasets

tf1=results[which(results$years==min1):which(results$years==max1),2]


datespan1=rep(paste(min1,"-", max1), length(tf1))
tf1=data.frame(Years=datespan1, Value=tf1)

tf2=results[which(results$years==min2):which(results$years==max2),2]

datespan2=rep(paste(min2,"-", max2), length(tf2))
tf2=data.frame(Years=datespan2,Value=tf2)

tf=rbind(tf1,tf2)


#plot the dataset
titl=paste("Time slice analysis at", station)
subtitl=paste("Comparison of annual minimum discharge values. Timeframes:", min1, "-", max1 , "and", min2, "-",max2,".")

plot=ggplot(tf, aes(x = Years, y = Value)) +            # Applying ggplot function
  geom_boxplot( color="blue",
                fill="blue",
                alpha=0.2,

                # Notch?
                #notch=TRUE,
                #notchwidth = 0.8,

                # custom outliers
                outlier.colour="red",
                outlier.fill="red",
                outlier.size=3) +ggtitle(titl, subtitle=subtitl)+xlab("Timespan [years]")+ylab(expression("Discharge [" ~ m^3/s *"]"))+ scale_fill_brewer(palette="BuPu")

return(plot)

}

