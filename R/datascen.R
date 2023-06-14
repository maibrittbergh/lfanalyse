#' datascen
#'
#' @description Function combines all projections in one tidy dataset (list). The function was written for the approach to combine different models in the analysis to reduce or understand uncertainities.
#' @param G_6p0_d
#' @param G_8p5_d
#' @param I_2p6_d
#' @param I_6p0_d
#' @param I_8p5_d
#' @param H_2p6_d
#' @param H_6p0_d
#' @param H_8p5_d
#' @param M_2p6_d
#' @param M_6p0_d
#' @param M_8p5_d
#' @param N_2p6_d
#' @param N_6p0_d
#' @param N_8p5_d
#' @param G_hist_d
#' @param I_hist_d
#' @param H_hist_d
#' @param N_hist_d
#' @param M_hist_d
#'
#' @return Returns a list in a tidy order. Each entry of the list describes one river and combines the date, the model and the scenario of the projection in a dataframe.
#' @examples \dontrun{mod_data=datascen(G_2p6_d, G_6p0_d, G_8p5_d, I_2p6_d, I_6p0_d, I_8p5_d, H_2p6_d, H_6p0_d, H_8p5_d, M_2p6_d, M_6p0_d, M_8p5_d, N_2p6_d, N_6p0_d, N_8p5_d, G_hist_d, I_hist_d, H_hist_d, N_hist_d, M_hist_d)}


datascen=function(G_2p6_d, G_6p0_d, G_8p5_d, I_2p6_d, I_6p0_d, I_8p5_d, H_2p6_d, H_6p0_d, H_8p5_d, M_2p6_d, M_6p0_d, M_8p5_d, N_2p6_d, N_6p0_d, N_8p5_d, G_hist_d, I_hist_d, H_hist_d, N_hist_d, M_hist_d){
  rivers=ncol(G_2p6_d)-1
  list <- vector(mode='list', length=length(rivers))



  ##### G-Projection
  for(i in 1:rivers){

    Value=G_2p6_d[i+1]
    scenario=rep("2p6",length(Value) )
    model=rep("G",length(Value) )
    date=G_2p6_d[1]

    list[[i]]=data.frame(date, Value, scenario, model)

  }

  names(list)=colnames(G_2p6_d)[-1]


  for(i in 1:rivers){

    Value=G_6p0_d[i+1]
    scenario=rep("6p0",length(Value) )
    model=rep("G",length(Value) )
    date=G_6p0_d[1]

    list[[i]]=rbind(list[[i]], data.frame(date, Value, scenario, model))

  }

  for(i in 1:rivers){

    Value=G_8p5_d[i+1]
    scenario=rep("8p5",length(Value) )
    model=rep("G",length(Value) )
    date=G_8p5_d[1]

    list[[i]]=rbind(list[[i]], data.frame(date, Value, scenario, model))

  }

  ##### I-Projection

  for(i in 1:rivers){

    Value=I_2p6_d[i+1]
    scenario=rep("2p6",length(Value) )
    model=rep("I",length(Value) )
    date=I_2p6_d[1]

    list[[i]]=data.frame(date, Value, scenario, model)

  }


  for(i in 1:rivers){

    Value=I_6p0_d[i+1]
    scenario=rep("6p0",length(Value) )
    model=rep("I",length(Value) )
    date=I_6p0_d[1]

    list[[i]]=rbind(list[[i]], data.frame(date, Value, scenario, model))

  }

  for(i in 1:rivers){

    Value=I_8p5_d[i+1]
    scenario=rep("8p5",length(Value) )
    model=rep("I",length(Value) )
    date=I_8p5_d[1]

    list[[i]]=rbind(list[[i]], data.frame(date, Value, scenario, model))

  }


  #### H-Projection

  for(i in 1:rivers){

    Value=H_2p6_d[i+1]
    scenario=rep("2p6",length(Value) )
    model=rep("H",length(Value) )
    date=H_2p6_d[1]

    list[[i]]=data.frame(date, Value, scenario, model)

  }


  for(i in 1:rivers){

    Value=H_6p0_d[i+1]
    scenario=rep("6p0",length(Value) )
    model=rep("H",length(Value) )
    date=H_6p0_d[1]

    list[[i]]=rbind(list[[i]], data.frame(date, Value, scenario, model))

  }

  for(i in 1:rivers){

    Value=H_8p5_d[i+1]
    scenario=rep("8p5",length(Value) )
    model=rep("H",length(Value) )
    date=H_8p5_d[1]

    list[[i]]=rbind(list[[i]], data.frame(date, Value, scenario, model))

  }

  #### M-Projection

  for(i in 1:rivers){

    Value=M_2p6_d[i+1]
    scenario=rep("2p6",length(Value) )
    model=rep("M",length(Value) )
    date=M_2p6_d[1]

    list[[i]]=data.frame(date, Value, scenario, model)

  }


  for(i in 1:rivers){

    Value=M_6p0_d[i+1]
    scenario=rep("6p0",length(Value) )
    model=rep("M",length(Value) )
    date=M_6p0_d[1]

    list[[i]]=rbind(list[[i]], data.frame(date, Value, scenario, model))

  }

  for(i in 1:rivers){

    Value=M_8p5_d[i+1]
    scenario=rep("8p5",length(Value) )
    model=rep("M",length(Value) )
    date=M_8p5_d[1]

    list[[i]]=rbind(list[[i]], data.frame(date, Value, scenario, model))

  }

  #### N-Projection
  for(i in 1:rivers){

    Value=N_2p6_d[i+1]
    scenario=rep("2p6",length(Value) )
    model=rep("N",length(Value) )
    date=N_2p6_d[1]

    list[[i]]=data.frame(date, Value, scenario, model)

  }


  for(i in 1:rivers){

    Value=N_6p0_d[i+1]
    scenario=rep("6p0",length(Value) )
    model=rep("N",length(Value) )
    date=N_6p0_d[1]

    list[[i]]=rbind(list[[i]], data.frame(date, Value, scenario, model))

  }

  for(i in 1:rivers){

    Value=N_8p5_d[i+1]
    scenario=rep("8p5",length(Value) )
    model=rep("N",length(Value) )
    date=N_8p5_d[1]

    list[[i]]=rbind(list[[i]], data.frame(date, Value, scenario, model))

  }

  ##### Including historical Data to list
  for(i in 1:rivers){

    Value=G_hist_d[i+1]
    scenario=rep("hist",length(Value) )
    model=rep("G",length(Value) )
    date=G_hist_d[1]

    list[[i]]=rbind( data.frame(date, Value, scenario, model),list[[i]])

  }






  for(i in 1:rivers){

    Value=I_hist_d[i+1]
    scenario=rep("hist",length(Value) )
    model=rep("I",length(Value) )
    date=I_hist_d[1]

    list[[i]]=rbind( data.frame(date, Value, scenario, model),list[[i]])

  }




  for(i in 1:rivers){

    Value=H_hist_d[i+1]
    scenario=rep("hist",length(Value) )
    model=rep("H",length(Value) )
    date=H_hist_d[1]

    list[[i]]=rbind( data.frame(date, Value, scenario, model),list[[i]])

  }


  for(i in 1:rivers){

    Value=M_hist_d[i+1]
    scenario=rep("hist",length(Value) )
    model=rep("M",length(Value) )
    date=M_hist_d[1]

    list[[i]]=rbind( data.frame(date, Value, scenario, model),list[[i]])

  }

  for(i in 1:rivers){

    Value=N_hist_d[i+1]
    scenario=rep("hist",length(Value) )
    model=rep("N",length(Value) )
    date=N_hist_d[1]

    list[[i]]=rbind( data.frame(date, Value, scenario, model),list[[i]])

  }




  return(list)

}
