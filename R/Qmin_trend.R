#' Trend of Annual Discharge Minimum Values since begin of Measurements
#'
#'@param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#'@param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#'@param mod numeric; possible input: 1,2,3. default value: 1; output of both: \link[zyp]{zyp.trend.vector}, \link[stats]{lm}. Defines the way to calculate intercept and slope. For mod=2  \link[zyp]{zyp.trend.vector} with PreWhitening by "yuepilon-method" is used. Sen-Slope-Approach used to define direction of the trend and the significance is  determined by Kendall's P-Value computed for the final detrendet time series. For mod=3: \link[stats]{lm} with a least squares approach is used.

#' @import ggplot2
#' @import zyp
#' @import stats
#'
#' @examples
#' \dontrun{ Qmin_trend(data, "COCHEM")}
#'
Qmin_trend=function(data, station, mod=1) {
  library(ggplot2)
  min_trend=function(data, station, mod= 1) {


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

    if (mod == 3){


      model=lm(q_min ~ years, results)  #least squares.lm to fit a linear model.
      intercept_ls=as.numeric(model$coefficients[1])
      slope_ls=as.numeric(model$coefficients[2])
      llm=list(intercept_ls, slope_ls)
      names(llm)=c("intercept_lm", "slope_lm")
      return(llm)

    }else if (mod == 2){
      mod=zyp.trend.vector(y=results$q_min, x=years,  method="yuepilon")  #
      intercept_zyp=as.numeric(mod[11])
      slope_zyp=as.numeric(mod[2])
      sig_zyp=as.numeric(mod[6])
      lzyp= list(intercept_zyp, slope_zyp, sig_zyp)

      names(lzyp)=c("intercept_zyp", "slope_zyp","sig_zyp")


      return(lzyp)



    }else{


      model=lm(q_min ~ years, results)  #least squares.lm to fit a linear model.
      intercept_ls=as.numeric(model$coefficients[1])
      slope_ls=as.numeric(model$coefficients[2])

      mod=zyp.trend.vector(results$q_min, x=years, method="yuepilon")  #
      intercept_zyp=as.numeric(mod[11])
      slope_zyp=as.numeric(mod[2])
      sig_zyp=as.numeric(mod[6])
      lb= list(intercept_zyp, slope_zyp, sig_zyp, intercept_ls, slope_ls)

      names(lb)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm")


      return(lb)
    }


  }



 nbr=which(names(data)==station)
  val=data[[nbr]]
  abs_min=min(data[[nbr]][,2])

  year_one=as.numeric(substring(as.character(data[[nbr]][1,1]),1,4))
  length=length(data[[nbr]][,1])
  last_year=as.numeric(substring(as.character(data[[nbr]][length,1]),1,4))
  years=c(year_one:last_year)
  l=length(years)
  q_min=rep(0, l)
  for ( i in 1:l){
    year=as.character(years[i])
  j=grep(year, data[[nbr]][,1])
    Val=data[[nbr]][,2][j]
    q_min[i]=min(Val)
  }
  results=data.frame(years, q_min)
  model= min_trend(data, station)


  if(mod==1){
    titl=paste("Yuepilon and Linear Trend of Minimum Values at",station)
    cap=paste('Slope: Trend- Sens Sloap:',round(model$slope_zyp,3),"Kendall's P-Value:", round(model$sig_zyp, 3),"Slope: Trend- Least Squares:", round(model$slope_lm,3))
    plot=ggplot(results)+geom_line(mapping=aes(x=years,y=q_min, group=1, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=paste("from", year_one, "to", last_year), x="Years" ,  y=expression('Minimum Discharge Value [m'^3*'/s]'), caption=cap)+
      geom_abline(aes(intercept = model$intercept_zyp, slope= model$slope_zyp,  col="b"), show.legend=TRUE)+
      geom_abline(aes(intercept= model$intercept_lm, slope=model$slope_lm,col="c"), show.legend=TRUE)+  scale_color_manual(name = "Legend:   ",
                                                                                                                           labels=c("Minimum values", "Trend - Sens Sloap",
                                                                                                                                    "Trend-Least Squares"), values=c("a"="#F8766D","b"= "#00BDD0", "c"="darkblue"), guide="legend")+ theme(legend.position = "bottom" )

  }else if (mod==2){

    titl=paste("Yuepilon  Trend of Minimum Values at",station)
    cap=paste('Slope: Trend- Sens Sloap:',round(model$slope_zyp,3), "Kendall's P-Value:", round(model$sig_zyp, 3))
    plot=ggplot(results)+geom_line(mapping=aes(x=years,y=q_min, group=1, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=paste("from", year_one, "to", last_year), x="Years" , y=expression('Minimum Discharge Value [m'^3*'/s]'), caption=cap)+
      geom_abline(aes(intercept = model$intercept_zyp, slope= model$slope_zyp,  col="b"), show.legend=TRUE)+
      scale_color_manual(name = "Legend:   ",
                         labels=c("Minimum values", "Trend - Sens Sloap"
                         ), values=c("a"="#F8766D","b"= "#00BDD0"), guide="legend")+ theme(legend.position = "bottom" )
  }else if(mod==3){

    titl=paste("Linear Trend of Minimum Values at",station)
    cap=paste('Slope: Trend- Least Squares:', round(model$slope_lm,3))
    plot=ggplot(results)+geom_line(mapping=aes(x=years,y=q_min, group=1, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=paste("from", year_one, "to", last_year), x="Years" ,  y=expression('Minimum Discharge Value [m'^3*'/s]'), caption=cap)+
      geom_abline(aes(intercept= model$intercept_lm, slope=model$slope_lm,col="c"), show.legend=TRUE)+  scale_color_manual(name = "Legend:   ",
                                                                                                                           labels=c("Minimum values",
                                                                                                                                    "Trend-Least Squares"), values=c("a"="#F8766D", "c"="darkblue"), guide="legend")+ theme(legend.position = "bottom" )
  }




  return(plot)


}
