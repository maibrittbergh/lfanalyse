#' Future Boxplot LowFlow Analysis
#'
#' @description Function Analyses 3 rivers with respect to the average annual discharge value, the NM7Q-Value and the Number of Days under the Q90 threshold. The Idea of the plot is to compare modelled data of the past and the future. For the future one can distinguish NF and FF scenarios.
#'
#'
#' @param rivernames character; vector of three river names, as they are listed in the dataset
#' @param sel_names character; vector of three river names, as they should appear in the final plot.
#' @param mod_data list; each entry of the list represents a river/station (catchement). For every catchment 5 different models were taken to compute historical and future discharge Values.
#' @param NF numerical; vector containing all the years that account for the near future. The function itself operates with hydrological years.
#' @param FF numerical; vector containing all the years that account for the far future. The function itself operates with hydrological years.
#' @param FFH numerical; vector FF minus maximum year.
#' @param P numerical; vector containing all the years that account for the past. The function itself operates with hydrological years.
#'
#' @return plot
#' @export
#'
#' @examples
#' \dontrun{bpplot=bpanalysis(rivernames=c("Moselle_3", "Neckar_3", "Main_3"), sel_names=c("Mosel", "Neckar", "Main"),  mod_data, NF=c(2040:2069),
#'                  FF=c(2070:2098),
#'                  FFH=c(2070:2097),
#'                 P=c(1970:1999) )}


bpanalysis=function(rivernames, sel_names, mod_data, NF,FF, FFH, P){

  a=mod_data[[rivernames[1]]]
  pasta=a[min(which(year(a$date)==P[1])):max(which(year(a$date)==max(P))),]
  period= rep("1.Hist", nrow(pasta))
  a_past=data.frame(cbind(pasta, period ))

  nfa=a[min(which(year(a$date)==NF[1])):max(which(year(a$date)==max(NF))),]
  period= rep("2.NF", nrow(nfa))
  a_nfa=data.frame(cbind(nfa, period ))
  ffa=a[min(which(year(a$date)==FF[1])):max(which(year(a$date)==max(FF))),]
  period= rep("3.FF", nrow(ffa))
  a_ffa=data.frame(cbind(ffa, period ))
  aset=data.frame(rbind(a_past, a_nfa, a_ffa))

  meth=rep(method[1], nrow(aset))
  sel_name=rep(sel_names[1],  nrow(aset))
  aset=data.frame(cbind(aset, meth, sel_name))
  colnames(aset)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )

  ### Method 1: hydrological year mean

  annualmean=function(data, year){


    min=paste(as.character(year), "-11-01")
    min=sub(" -", "-",min)
    max=paste(as.character(year+1), "-10-31")
    max=sub(" -", "-",max)
    mindate=grep(min,data[,1])
    maxdate=grep(max,data[,1])
    Val=data[,2]

    datayear=data[mindate:maxdate,]

    valyear=datayear[,2]
    le=length(mindate:maxdate)

    meanval=round(mean(valyear),2)





  }

  #### Past Mean
  Past_mean= rep(0,length(P))
  data1=filter(aset, scenario=="hist")
  for( i in 1:length(P)){

    Past_mean[i]=as.numeric(annualmean( data1, P[i] ))
  }

  meanPast=data.frame(P,as.numeric(Past_mean),rep("hist", length(P)), rep("all", length(P)), rep("1.Hist", length(P)), rep("Abflussmittel", length(P)), rep(sel_names[1], length(P)))
  colnames(meanPast)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanPast

  ### NF Scnearios: Annual Mean
  #### 2p6 Mean NF
  NF2p6_mean= rep(0,length(NF))
  data2p6=filter(aset, scenario=="2p6")
  for( i in 1:length(NF)){

    NF2p6_mean[i]=as.numeric(annualmean( data2p6, NF[i] ))
  }

  meanNF2p6=data.frame(NF,as.numeric(NF2p6_mean),rep("2p6", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Abflussmittel", length(NF)), rep(sel_names[1], length(NF)))
  colnames( meanNF2p6)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanNF2p6

  #### 6p0 Mean NF
  NF6p0_mean= rep(0,length(NF))
  data6p0=filter(aset, scenario=="6p0")
  for( i in 1:length(NF)){

    NF6p0_mean[i]=as.numeric(annualmean( data6p0, NF[i] ))
  }

  meanNF6p0=data.frame(NF,as.numeric(NF6p0_mean),rep("6p0", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Abflussmittel", length(NF)), rep(sel_names[1], length(NF)))
  colnames( meanNF6p0)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanNF6p0


  #### 8p5 Mean FF
  NF8p5_mean= rep(0,length(NF))
  data8p5=filter(aset, scenario=="8p5")
  for( i in 1:length(NF)){

    NF8p5_mean[i]=as.numeric(annualmean( data8p5, NF[i] ))
  }

  meanNF8p5=data.frame(NF,as.numeric(NF8p5_mean),rep("8p5", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Abflussmittel", length(NF)), rep(sel_names[1], length(NF)))
  colnames( meanNF8p5)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanNF8p5



  ### FF Scnearios: Annual Mean
  #### 2p6 Mean FF
  FF2p6_mean= rep(0,length(FF))
  data2p6=filter(aset, scenario=="2p6")
  for( i in 1:length(FF)){

    FF2p6_mean[i]=as.numeric(annualmean( data2p6, FF[i] ))
  }

  meanFF2p6=data.frame(FF,as.numeric(FF2p6_mean),rep("2p6", length(FF)), rep("all", length(FF)), rep("3.FF", length(FF)), rep("Abflussmittel", length(FF)), rep(sel_names[1], length(FF)))
  colnames(meanFF2p6)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanFF2p6

  #### 6p0 Mean FF
  FF6p0_mean= rep(0,length(FF))
  data6p0=filter(aset, scenario=="6p0")
  for( i in 1:length(FF)){

    FF6p0_mean[i]=as.numeric(annualmean( data6p0, FF[i] ))
  }

  meanFF6p0=data.frame(FF,as.numeric(FF6p0_mean),rep("6p0", length(FF)), rep("all", length(FF)), rep("3.FF", length(FF)), rep("Abflussmittel", length(FF)), rep(sel_names[1], length(FF)))
  colnames(meanFF6p0)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanFF6p0


  #### 8p5 Mean FF
  FF8p5_mean= rep(0,length(FFH))
  data8p5=filter(aset, scenario=="8p5")
  for( i in 1:length(FFH)){

    FF8p5_mean[i]=as.numeric(annualmean( data8p5, FFH[i] ))
  }

  meanFF8p5=data.frame(FFH,as.numeric(FF8p5_mean),rep("8p5", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("Abflussmittel", length(FFH)), rep(sel_names[1], length(FFH)))
  colnames(meanFF8p5)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanFF8p5




  meanh1=data.frame(rbind(   meanPast, meanNF2p6,  meanNF6p0,  meanNF8p5,  meanFF2p6,  meanFF6p0,  meanFF8p5))




  #### Method 2 : Annual Min

  NMxQ= function(x, data,  year){


    yearmax=as.character(year+1)
    yearmin=as.character(year)


    min=paste(yearmin,"-", "11-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "10-31")
    max=sub(" - ", "-",max)
    start=grep(min, data[,1])
    end=grep(max, data[,1])


    data=data[start:end, ]
    l=nrow(data)

    le=l-x
    le
    Nmxq=rep(0, le)
    for ( i in 1:le){
      Nmxq[i]=round(mean(data[i:(i+x), 2]),0)


    }
    return(min(Nmxq))
  }

  ##### Past NMxQ



  Past_NMXQ=rep(0,length(P))
  data1=filter(aset, scenario=="hist")

  for( i in 1:length(P)){

    Past_NMXQ[i]=as.numeric(NMxQ(7, data1, P[i] ))
  }
  #c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  nm7qPast=data.frame(P,as.numeric(Past_NMXQ),rep("hist", length(P)), rep("all", length(P)), rep("1.Hist", length(P)), rep("NM7Q", length(P)), rep(sel_names[1], length(P)))
  colnames(nm7qPast)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  nm7qPast

  #### NF NMXQ
  ##### Scen 2p6

  NF2p6_NMXQ=rep(0,length(NF))
  data2p6=filter(aset, scenario=="2p6")


  for( i in 1:length(NF)){

    NF2p6_NMXQ[i]=as.numeric(NMxQ(7, data2p6, NF[i] ))

  }


  nm7qNF2p6=data.frame(NF,NF2p6_NMXQ,rep("2p6", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("NM7Q", length(NF)), rep(sel_names[1], length(NF)))
  colnames(nm7qNF2p6)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen 6p0



  NF6p0_NMXQ=rep(0,length(NF))
  data6p0=filter(aset, scenario=="6p0")


  for( i in 1:length(NF)){

    NF6p0_NMXQ[i]=as.numeric(NMxQ(7, data6p0, NF[i] ))

  }


  nm7qNF6p0=data.frame(NF,NF6p0_NMXQ,rep("6p0", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("NM7Q", length(NF)), rep(sel_names[1], length(NF)))
  colnames(nm7qNF6p0)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen 8p5



  NF8p5_NMXQ=rep(0,length(NF))
  data8p5=filter(aset, scenario=="8p5")


  for( i in 1:length(NF)){

    NF8p5_NMXQ[i]=as.numeric(NMxQ(7, data8p5, NF[i] ))

  }


  nm7qNF8p5=data.frame(NF,NF8p5_NMXQ,rep("8p5", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("NM7Q", length(NF)), rep(sel_names[1], length(NF)))
  colnames(nm7qNF8p5)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )

  #### FF NMXQ
  ##### Scen 2p6

  FF2p6_NMXQ=rep(0,length(FFH))
  data2p6=filter(aset, scenario=="2p6")

  for( i in 1:length(FFH)){

    FF2p6_NMXQ[i]=NMxQ(7, data2p6, FFH[i] )

  }


  nm7qFF2p6=data.frame(FFH,FF2p6_NMXQ,rep("2p6", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("NM7Q", length(FFH)), rep(sel_names[1], length(FFH)))
  colnames(nm7qFF2p6)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen 6p0



  FF6p0_NMXQ=rep(0,length(FFH))
  data6p0=filter(aset, scenario=="6p0")


  for( i in 1:length(FFH)){

    FF6p0_NMXQ[i]=as.numeric(NMxQ(7, data6p0, FFH[i] ))

  }


  nm7qFF6p0=data.frame(FFH,FF6p0_NMXQ,rep("6p0", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("NM7Q", length(FFH)), rep(sel_names[1], length(FFH)))
  colnames(nm7qFF6p0)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen 8p5



  FF8p5_NMXQ=rep(0,length(FFH))
  data8p5=filter(aset, scenario=="8p5")


  for( i in 1:length(FFH)){

    FF8p5_NMXQ[i]=as.numeric(NMxQ(7, data8p5, FFH[i] ))

  }


  nm7qFF8p5=data.frame(FFH,FF8p5_NMXQ,rep("8p5", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("NM7Q", length(FFH)), rep(sel_names[1], length(FFH)))
  colnames(nm7qFF8p5)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )





  nm7qa1=data.frame(rbind(nm7qPast, nm7qNF2p6, nm7qNF6p0, nm7qNF8p5, nm7qFF2p6, nm7qFF6p0, nm7qFF8p5))


  # FF2=filter(dataa, period=="2.FF")



  #### Including Duration of longest Period under Q90 Threshold

  threshploty=function(data, station, U,year){


    min=paste(as.character(year), "-11-01")
    min=sub(" -", "-",min)
    max=paste(as.character(year+1), "-10-31")
    max=sub(" -", "-",max)
    mindate=grep(min,data[,1])
    maxdate=grep(max,data[,1])
    Val=data[,2]
    datayear=data[mindate:maxdate,]
    valyear=datayear[,2]
    le=length(mindate:maxdate)


    #calculate deficite

    g=which(valyear<U)
    vals=valyear[g]
    suml=length(g)




    return(suml)



  }



  deficite=function(data, U,year){

    min=paste(as.character(year), "-11-01")
    min=sub(" -", "-",min)
    max=paste(as.character(year+1), "-10-31")
    max=sub(" -", "-",max)
    mindate=grep(min,data[,1])
    maxdate=grep(max,data[,1])
    Val=data[,2]

    datayear=data[mindate:maxdate,]

    valyear=datayear[,2]
    le=length(mindate:maxdate)







    #calculate deficite
    if(any(valyear<U)==F){return(A=0)}else{
      g=which(valyear<U)
      vals=valyear[g]
      suml=length(g)
      sum=rep(0, suml)
      for(i in 1:suml){
        sum[i]=U-vals[i]
      }
      deficite=sum(sum)
      A=deficite



    }
    return(A)
  }
  data12=data.frame(filter(aset, scenario=="hist"))[,1:2]
  Q90=quantile(data12[,2], probs=0.10)[[1]]


  #### historic dataset


  P_deficite=rep(0, length(P))




  for( i in 1:length(P)){
    # P_deficite[i]=as.numeric(deficite(U=Q90,data=data12, year=P[i]))
    thresh=threshploty(data=data12 , U=Q90,year=P[i])
    P_deficite[i]=thresh

  }


  #  threshploty=function(data, station, U,year){



  def_P=data.frame(P,P_deficite,rep("hist", length(P)), rep("all", length(P)), rep("1.Hist", length(P)), rep("Tage < Q90", length(P)), rep(sel_names[1], length(P)))
  colnames(def_P)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  #### NF Scenario

  ##### Scen: 2p6
  NF2p6_def=rep(0,length(NF))
  data2p6=filter(aset, scenario=="2p6")


  for( i in 1:length(NF)){


    thresh=threshploty(U=Q90,data=data2p6, year=NF[i])
    NF2p6_def[i]=thresh

  }


  def_2p6_NF=data.frame(NF,NF2p6_def,rep("2p6", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Tage < Q90", length(NF)), rep(sel_names[1], length(NF)))
  colnames( def_2p6_NF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )

  ##### Scen: 6p0
  NF6p0_def=rep(0,length(NF))
  data6p0=filter(aset, scenario=="6p0")


  for( i in 1:length(NF)){

    thresh=threshploty(U=Q90,data=data6p0, year=NF[i])
    NF6p0_def[i]=thresh

  }


  def_6p0_NF=data.frame(NF,NF6p0_def,rep("6p0", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Tage < Q90", length(NF)), rep(sel_names[1], length(NF)))
  colnames( def_6p0_NF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen: 8p5
  NF8p5_def=rep(0,length(NF))
  data8p5=filter(aset, scenario=="8p5")


  for( i in 1:length(NF)){


    thresh=threshploty(U=Q90,data=data8p5, year=NF[i])
    NF8p5_def[i]=thresh


  }


  def_8p5_NF=data.frame(NF,NF8p5_def,rep("8p5", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Tage < Q90", length(NF)), rep(sel_names[1], length(NF)))
  colnames( def_8p5_NF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ### FF Scenario

  ##### Scen: 2p6
  FF2p6_def=rep(0,length(FFH))
  data2p6=filter(aset, scenario=="2p6")


  for( i in 1:length(FFH)){



    thresh=threshploty(U=Q90,data=data2p6, year=FFH[i])
    FF2p6_def[i]=thresh

  }


  def_2p6_FF=data.frame(FFH,FF2p6_def,rep("2p6", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("Tage < Q90", length(FFH)), rep(sel_names[1], length(FFH)))
  colnames( def_2p6_FF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )

  ##### Scen: 6p0
  FF6p0_def=rep(0,length(FFH))
  data6p0=filter(aset, scenario=="6p0")


  for( i in 1:length(FFH)){




    thresh=threshploty(U=Q90,data=data6p0, year=FFH[i])
    FF6p0_def[i]=thresh

  }


  def_6p0_FF=data.frame(FFH,FF6p0_def,rep("6p0", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("Tage < Q90", length(FFH)), rep(sel_names[1], length(FFH)))
  colnames( def_6p0_FF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen: 8p5
  FF8p5_def=rep(0,length(FFH))
  data8p5=filter(aset, scenario=="8p5")



  for( i in 1:length(FFH)){



    thresh=threshploty(U=Q90,data=data8p5, year=FFH[i])
    FF8p5_def[i]=thresh


  }



  def_8p5_FF=data.frame(FFH,FF8p5_def,rep("8p5", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("Tage < Q90", length(FFH)), rep(sel_names[1], length(FFH)))
  colnames( def_8p5_FF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  #### Combining deficite dataset

  defa=data.frame(rbind( def_P,def_2p6_NF, def_6p0_NF, def_8p5_NF, def_2p6_FF, def_6p0_FF, def_8p5_FF))



  defamax=which(defa$Value== max(defa$Value))
  defa1=defa[-defamax,]

  # first results
  dataa1=data.frame(rbind(meanh1[,-1], nm7qa1[,-1], defa1[,-1]))

  #### Including the other rivers:


  a=mod_data[[rivernames[2]]]
  pasta=a[min(which(year(a$date)==P[1])):max(which(year(a$date)==max(P))),]
  period= rep("1.Hist", nrow(pasta))
  a_past=data.frame(cbind(pasta, period ))

  nfa=a[min(which(year(a$date)==NF[1])):max(which(year(a$date)==max(NF))),]
  period= rep("2.NF", nrow(nfa))
  a_nfa=data.frame(cbind(nfa, period ))
  ffa=a[min(which(year(a$date)==FF[1])):max(which(year(a$date)==max(FF))),]
  period= rep("3.FF", nrow(ffa))
  a_ffa=data.frame(cbind(ffa, period ))
  aset=data.frame(rbind(a_past, a_nfa, a_ffa))

  meth=rep(method[1], nrow(aset))
  sel_name=rep(sel_names[2],  nrow(aset))
  aset=data.frame(cbind(aset, meth, sel_name))
  colnames(aset)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )




  #### Past Mean
  Past_mean= rep(0,length(P))
  data1=filter(aset, scenario=="hist")
  for( i in 1:length(P)){

    Past_mean[i]=as.numeric(annualmean( data1, P[i] ))
  }

  meanPast=data.frame(P,as.numeric(Past_mean),rep("hist", length(P)), rep("all", length(P)), rep("1.Hist", length(P)), rep("Abflussmittel", length(P)), rep(sel_names[2], length(P)))
  colnames(meanPast)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanPast

  ### NF Scnearios: Annual Mean
  #### 2p6 Mean NF
  NF2p6_mean= rep(0,length(NF))
  data2p6=filter(aset, scenario=="2p6")
  for( i in 1:length(NF)){

    NF2p6_mean[i]=as.numeric(annualmean( data2p6, NF[i] ))
  }

  meanNF2p6=data.frame(NF,as.numeric(NF2p6_mean),rep("2p6", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Abflussmittel", length(NF)), rep(sel_names[2], length(NF)))
  colnames( meanNF2p6)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanNF2p6

  #### 6p0 Mean NF
  NF6p0_mean= rep(0,length(NF))
  data6p0=filter(aset, scenario=="6p0")
  for( i in 1:length(NF)){

    NF6p0_mean[i]=as.numeric(annualmean( data6p0, NF[i] ))
  }

  meanNF6p0=data.frame(NF,as.numeric(NF6p0_mean),rep("6p0", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Abflussmittel", length(NF)), rep(sel_names[2], length(NF)))
  colnames( meanNF6p0)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanNF6p0


  #### 8p5 Mean FF
  NF8p5_mean= rep(0,length(NF))
  data8p5=filter(aset, scenario=="8p5")
  for( i in 1:length(NF)){

    NF8p5_mean[i]=as.numeric(annualmean( data8p5, NF[i] ))
  }

  meanNF8p5=data.frame(NF,as.numeric(NF8p5_mean),rep("8p5", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Abflussmittel", length(NF)), rep(sel_names[2], length(NF)))
  colnames( meanNF8p5)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanNF8p5



  ### FF Scnearios: Annual Mean
  #### 2p6 Mean FF
  FF2p6_mean= rep(0,length(FF))
  data2p6=filter(aset, scenario=="2p6")
  for( i in 1:length(FF)){

    FF2p6_mean[i]=as.numeric(annualmean( data2p6, FF[i] ))
  }

  meanFF2p6=data.frame(FF,as.numeric(FF2p6_mean),rep("2p6", length(FF)), rep("all", length(FF)), rep("3.FF", length(FF)), rep("Abflussmittel", length(FF)), rep(sel_names[2], length(FF)))
  colnames(meanFF2p6)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanFF2p6

  #### 6p0 Mean FF
  FF6p0_mean= rep(0,length(FF))
  data6p0=filter(aset, scenario=="6p0")
  for( i in 1:length(FF)){

    FF6p0_mean[i]=as.numeric(annualmean( data6p0, FF[i] ))
  }

  meanFF6p0=data.frame(FF,as.numeric(FF6p0_mean),rep("6p0", length(FF)), rep("all", length(FF)), rep("3.FF", length(FF)), rep("Abflussmittel", length(FF)), rep(sel_names[2], length(FF)))
  colnames(meanFF6p0)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanFF6p0


  #### 8p5 Mean FF
  FF8p5_mean= rep(0,length(FFH))
  data8p5=filter(aset, scenario=="8p5")
  for( i in 1:length(FFH)){

    FF8p5_mean[i]=as.numeric(annualmean( data8p5, FFH[i] ))
  }

  meanFF8p5=data.frame(FFH,as.numeric(FF8p5_mean),rep("8p5", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("Abflussmittel", length(FFH)), rep(sel_names[2], length(FFH)))
  colnames(meanFF8p5)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanFF8p5




  meanh2=data.frame(rbind(   meanPast, meanNF2p6,  meanNF6p0,  meanNF8p5,  meanFF2p6,  meanFF6p0,  meanFF8p5))








  #### Method 2 : Annual Min

  NMxQ= function(x, data,  year){


    yearmax=as.character(year+1)
    yearmin=as.character(year)


    min=paste(yearmin,"-", "11-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "10-31")
    max=sub(" - ", "-",max)
    start=grep(min, data[,1])
    end=grep(max, data[,1])


    data=data[start:end, ]
    l=nrow(data)

    le=l-x
    le
    Nmxq=rep(0, le)
    for ( i in 1:le){
      Nmxq[i]=round(mean(data[i:(i+x), 2]),0)


    }
    return(min(Nmxq))
  }

  ##### Past NMxQ



  Past_NMXQ=rep(0,length(P))
  data1=filter(aset, scenario=="hist")

  for( i in 1:length(P)){

    Past_NMXQ[i]=as.numeric(NMxQ(7, data1, P[i] ))
  }
  #c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  nm7qPast=data.frame(P,as.numeric(Past_NMXQ),rep("hist", length(P)), rep("all", length(P)), rep("1.Hist", length(P)), rep("NM7Q", length(P)), rep(sel_names[2], length(P)))
  colnames(nm7qPast)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  nm7qPast

  #### NF NMXQ
  ##### Scen 2p6

  NF2p6_NMXQ=rep(0,length(NF))
  data2p6=filter(aset, scenario=="2p6")


  for( i in 1:length(NF)){

    NF2p6_NMXQ[i]=as.numeric(NMxQ(7, data2p6, NF[i] ))

  }


  nm7qNF2p6=data.frame(NF,NF2p6_NMXQ,rep("2p6", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("NM7Q", length(NF)), rep(sel_names[2], length(NF)))
  colnames(nm7qNF2p6)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen 6p0



  NF6p0_NMXQ=rep(0,length(NF))
  data6p0=filter(aset, scenario=="6p0")


  for( i in 1:length(NF)){

    NF6p0_NMXQ[i]=as.numeric(NMxQ(7, data6p0, NF[i] ))

  }


  nm7qNF6p0=data.frame(NF,NF6p0_NMXQ,rep("6p0", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("NM7Q", length(NF)), rep(sel_names[2], length(NF)))
  colnames(nm7qNF6p0)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen 8p5



  NF8p5_NMXQ=rep(0,length(NF))
  data8p5=filter(aset, scenario=="8p5")


  for( i in 1:length(NF)){

    NF8p5_NMXQ[i]=as.numeric(NMxQ(7, data8p5, NF[i] ))

  }


  nm7qNF8p5=data.frame(NF,NF8p5_NMXQ,rep("8p5", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("NM7Q", length(NF)), rep(sel_names[2], length(NF)))
  colnames(nm7qNF8p5)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )

  #### FF NMXQ
  ##### Scen 2p6

  FF2p6_NMXQ=rep(0,length(FFH))
  data2p6=filter(aset, scenario=="2p6")

  for( i in 1:length(FFH)){

    FF2p6_NMXQ[i]=NMxQ(7, data2p6, FFH[i] )

  }


  nm7qFF2p6=data.frame(FFH,FF2p6_NMXQ,rep("2p6", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("NM7Q", length(FFH)), rep(sel_names[2], length(FFH)))
  colnames(nm7qFF2p6)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen 6p0



  FF6p0_NMXQ=rep(0,length(FFH))
  data6p0=filter(aset, scenario=="6p0")


  for( i in 1:length(FFH)){

    FF6p0_NMXQ[i]=as.numeric(NMxQ(7, data6p0, FFH[i] ))

  }


  nm7qFF6p0=data.frame(FFH,FF6p0_NMXQ,rep("6p0", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("NM7Q", length(FFH)), rep(sel_names[2], length(FFH)))
  colnames(nm7qFF6p0)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen 8p5



  FF8p5_NMXQ=rep(0,length(FFH))
  data8p5=filter(aset, scenario=="8p5")


  for( i in 1:length(FFH)){

    FF8p5_NMXQ[i]=as.numeric(NMxQ(7, data8p5, FFH[i] ))

  }


  nm7qFF8p5=data.frame(FFH,FF8p5_NMXQ,rep("8p5", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("NM7Q", length(FFH)), rep(sel_names[2], length(FFH)))
  colnames(nm7qFF8p5)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )





  nm7qa2=data.frame(rbind(nm7qPast, nm7qNF2p6, nm7qNF6p0, nm7qNF8p5, nm7qFF2p6, nm7qFF6p0, nm7qFF8p5))




  # FF2=filter(dataa, period=="2.FF")



  #### Including Duration of longest Period under Q90 Threshold
  deficite=function(data, U,year){

    min=paste(as.character(year), "-11-01")
    min=sub(" -", "-",min)
    max=paste(as.character(year+1), "-10-31")
    max=sub(" -", "-",max)
    mindate=grep(min,data[,1])
    maxdate=grep(max,data[,1])
    Val=data[,2]

    datayear=data[mindate:maxdate,]

    valyear=datayear[,2]
    le=length(mindate:maxdate)







    #calculate deficite
    if(any(valyear<U)==F){return(A=0)}else{
      g=which(valyear<U)
      vals=valyear[g]
      suml=length(g)
      sum=rep(0, suml)
      for(i in 1:suml){
        sum[i]=U-vals[i]
      }
      deficite=sum(sum)
      A=deficite



    }
    return(A)
  }
  data12=data.frame(filter(aset, scenario=="hist"))[,1:2]
  Q90=quantile(data12[,2], probs=0.10)[[1]]


  #### historic dataset


  P_deficite=rep(0, length(P))


  for( i in 1:length(P)){

    #  P_deficite[i]=as.numeric(deficite(U=Q90,data=data12, year=P[i]))

    thresh=threshploty(U=Q90,data=data12, year=P[i])
    P_deficite[i]=thresh
  }


  def_P=data.frame(P,P_deficite,rep("hist", length(P)), rep("all", length(P)), rep("1.Hist", length(P)), rep("Tage < Q90", length(P)), rep(sel_names[2], length(P)))
  colnames(def_P)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  #### NF Scenario

  ##### Scen: 2p6
  NF2p6_def=rep(0,length(NF))
  data2p6=filter(aset, scenario=="2p6")


  for( i in 1:length(NF)){

    #NF2p6_def[i]=as.numeric(deficite(U=Q90,data=data2p6, year=NF[i]))
    thresh=threshploty(U=Q90,data=data2p6, year=NF[i])
    NF2p6_def[i]=thresh

  }


  def_2p6_NF=data.frame(NF,NF2p6_def,rep("2p6", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Tage < Q90", length(NF)), rep(sel_names[2], length(NF)))
  colnames( def_2p6_NF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )

  ##### Scen: 6p0
  NF6p0_def=rep(0,length(NF))
  data6p0=filter(aset, scenario=="6p0")


  for( i in 1:length(NF)){

    #NF6p0_def[i]=as.numeric(deficite(U=Q90,data=data6p0, year=NF[i]))
    thresh=threshploty(U=Q90,data=data6p0, year=NF[i])
    NF6p0_def[i]=thresh

  }


  def_6p0_NF=data.frame(NF,NF6p0_def,rep("6p0", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Tage < Q90", length(NF)), rep(sel_names[2], length(NF)))
  colnames( def_6p0_NF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen: 8p5
  NF8p5_def=rep(0,length(NF))
  data8p5=filter(aset, scenario=="8p5")


  for( i in 1:length(NF)){

    # NF8p5_def[i]=as.numeric(deficite(U=Q90,data=data8p5, year=NF[i]))
    thresh=threshploty(U=Q90,data=data8p5, year=NF[i])
    NF8p5_def[i]=thresh


  }


  def_8p5_NF=data.frame(NF,NF8p5_def,rep("8p5", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Tage < Q90", length(NF)), rep(sel_names[2], length(NF)))
  colnames( def_8p5_NF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ### FF Scenario

  ##### Scen: 2p6
  FF2p6_def=rep(0,length(FFH))
  data2p6=filter(aset, scenario=="2p6")


  for( i in 1:length(FFH)){

    #FF2p6_def[i]=as.numeric(deficite(U=Q90,data=data2p6, year=FFH[i]))

    thresh=threshploty(U=Q90,data=data2p6, year=FFH[i])
    FF2p6_def[i]=thresh

  }


  def_2p6_FF=data.frame(FFH,FF2p6_def,rep("2p6", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("Tage < Q90", length(FFH)), rep(sel_names[2], length(FFH)))
  colnames( def_2p6_FF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )

  ##### Scen: 6p0
  FF6p0_def=rep(0,length(FFH))
  data6p0=filter(aset, scenario=="6p0")


  for( i in 1:length(FFH)){

    # FF6p0_def[i]=as.numeric(deficite(U=Q90,data=data6p0, year=FFH[i]))
    thresh=threshploty(U=Q90,data=data6p0, year=FFH[i])
    FF6p0_def[i]=thresh

  }


  def_6p0_FF=data.frame(FFH,FF6p0_def,rep("6p0", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("Tage < Q90", length(FFH)), rep(sel_names[2], length(FFH)))
  colnames( def_6p0_FF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen: 8p5
  FF8p5_def=rep(0,length(FFH))
  data8p5=filter(aset, scenario=="8p5")


  for( i in 1:length(FFH)){

    #   FF8p5_def[i]=as.numeric(deficite(U=Q90,data=data8p5, year=FFH[i]))
    thresh=threshploty(U=Q90,data=data8p5, year=FFH[i])
    FF8p5_def[i]=thresh

  }


  def_8p5_FF=data.frame(FFH,FF8p5_def,rep("8p5", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("Tage < Q90", length(FFH)), rep(sel_names[2], length(FFH)))
  colnames( def_8p5_FF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  #### Combining deficite dataset

  defa=data.frame(rbind( def_P,def_2p6_NF, def_6p0_NF, def_8p5_NF, def_2p6_FF, def_6p0_FF, def_8p5_FF))



  defamax=which(defa$Value== max(defa$Value))
  defa2=defa[-defamax,]



  dataa2=data.frame(rbind(meanh2[,-1], nm7qa2[,-1], defa2[,-1]))




  ##### Including River 3

  a=mod_data[[rivernames[3]]]
  pasta=a[min(which(year(a$date)==P[1])):max(which(year(a$date)==max(P))),]
  period= rep("1.Hist", nrow(pasta))
  a_past=data.frame(cbind(pasta, period ))

  nfa=a[min(which(year(a$date)==NF[1])):max(which(year(a$date)==max(NF))),]
  period= rep("2.NF", nrow(nfa))
  a_nfa=data.frame(cbind(nfa, period ))
  ffa=a[min(which(year(a$date)==FF[1])):max(which(year(a$date)==max(FF))),]
  period= rep("3.FF", nrow(ffa))
  a_ffa=data.frame(cbind(ffa, period ))
  aset=data.frame(rbind(a_past, a_nfa, a_ffa))

  meth=rep(method[1], nrow(aset))
  sel_name=rep(sel_names[3],  nrow(aset))
  aset=data.frame(cbind(aset, meth, sel_name))
  colnames(aset)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )




  #### Past Mean
  Past_mean= rep(0,length(P))
  data1=filter(aset, scenario=="hist")
  for( i in 1:length(P)){

    Past_mean[i]=as.numeric(annualmean( data1, P[i] ))
  }

  meanPast=data.frame(P,as.numeric(Past_mean),rep("hist", length(P)), rep("all", length(P)), rep("1.Hist", length(P)), rep("Abflussmittel", length(P)), rep(sel_names[3], length(P)))
  colnames(meanPast)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanPast

  ### NF Scnearios: Annual Mean
  #### 2p6 Mean NF
  NF2p6_mean= rep(0,length(NF))
  data2p6=filter(aset, scenario=="2p6")
  for( i in 1:length(NF)){

    NF2p6_mean[i]=as.numeric(annualmean( data2p6, NF[i] ))
  }

  meanNF2p6=data.frame(NF,as.numeric(NF2p6_mean),rep("2p6", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Abflussmittel", length(NF)), rep(sel_names[3], length(NF)))
  colnames( meanNF2p6)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanNF2p6

  #### 6p0 Mean NF
  NF6p0_mean= rep(0,length(NF))
  data6p0=filter(aset, scenario=="6p0")
  for( i in 1:length(NF)){

    NF6p0_mean[i]=as.numeric(annualmean( data6p0, NF[i] ))
  }

  meanNF6p0=data.frame(NF,as.numeric(NF6p0_mean),rep("6p0", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Abflussmittel", length(NF)), rep(sel_names[3], length(NF)))
  colnames( meanNF6p0)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanNF6p0


  #### 8p5 Mean FF
  NF8p5_mean= rep(0,length(NF))
  data8p5=filter(aset, scenario=="8p5")
  for( i in 1:length(NF)){

    NF8p5_mean[i]=as.numeric(annualmean( data8p5, NF[i] ))
  }

  meanNF8p5=data.frame(NF,as.numeric(NF8p5_mean),rep("8p5", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Abflussmittel", length(NF)), rep(sel_names[3], length(NF)))
  colnames( meanNF8p5)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanNF8p5



  ### FF Scnearios: Annual Mean
  #### 2p6 Mean FF
  FF2p6_mean= rep(0,length(FF))
  data2p6=filter(aset, scenario=="2p6")
  for( i in 1:length(FF)){

    FF2p6_mean[i]=as.numeric(annualmean( data2p6, FF[i] ))
  }

  meanFF2p6=data.frame(FF,as.numeric(FF2p6_mean),rep("2p6", length(FF)), rep("all", length(FF)), rep("3.FF", length(FF)), rep("Abflussmittel", length(FF)), rep(sel_names[3], length(FF)))
  colnames(meanFF2p6)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanFF2p6

  #### 6p0 Mean FF
  FF6p0_mean= rep(0,length(FF))
  data6p0=filter(aset, scenario=="6p0")
  for( i in 1:length(FF)){

    FF6p0_mean[i]=as.numeric(annualmean( data6p0, FF[i] ))
  }

  meanFF6p0=data.frame(FF,as.numeric(FF6p0_mean),rep("6p0", length(FF)), rep("all", length(FF)), rep("3.FF", length(FF)), rep("Abflussmittel", length(FF)), rep(sel_names[3], length(FF)))
  colnames(meanFF6p0)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanFF6p0


  #### 8p5 Mean FF
  FF8p5_mean= rep(0,length(FFH))
  data8p5=filter(aset, scenario=="8p5")
  for( i in 1:length(FFH)){

    FF8p5_mean[i]=as.numeric(annualmean( data8p5, FFH[i] ))
  }

  meanFF8p5=data.frame(FFH,as.numeric(FF8p5_mean),rep("8p5", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("Abflussmittel", length(FFH)), rep(sel_names[3], length(FFH)))
  colnames(meanFF8p5)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  meanFF8p5




  meanh3=data.frame(rbind(   meanPast, meanNF2p6,  meanNF6p0,  meanNF8p5,  meanFF2p6,  meanFF6p0,  meanFF8p5))




  #### Method 2 : Annual Min

  NMxQ= function(x, data,  year){


    yearmax=as.character(year+1)
    yearmin=as.character(year)


    min=paste(yearmin,"-", "11-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "10-31")
    max=sub(" - ", "-",max)
    start=grep(min, data[,1])
    end=grep(max, data[,1])


    data=data[start:end, ]
    l=nrow(data)

    le=l-x
    le
    Nmxq=rep(0, le)
    for ( i in 1:le){
      Nmxq[i]=round(mean(data[i:(i+x), 2]),0)


    }
    return(min(Nmxq))
  }

  ##### Past NMxQ



  Past_NMXQ=rep(0,length(P))
  data1=filter(aset, scenario=="hist")

  for( i in 1:length(P)){

    Past_NMXQ[i]=as.numeric(NMxQ(7, data1, P[i] ))
  }
  #c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  nm7qPast=data.frame(P,as.numeric(Past_NMXQ),rep("hist", length(P)), rep("all", length(P)), rep("1.Hist", length(P)), rep("NM7Q", length(P)), rep(sel_names[3], length(P)))
  colnames(nm7qPast)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )
  nm7qPast

  #### NF NMXQ
  ##### Scen 2p6

  NF2p6_NMXQ=rep(0,length(NF))
  data2p6=filter(aset, scenario=="2p6")


  for( i in 1:length(NF)){

    NF2p6_NMXQ[i]=as.numeric(NMxQ(7, data2p6, NF[i] ))

  }


  nm7qNF2p6=data.frame(NF,NF2p6_NMXQ,rep("2p6", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("NM7Q", length(NF)), rep(sel_names[3], length(NF)))
  colnames(nm7qNF2p6)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen 6p0



  NF6p0_NMXQ=rep(0,length(NF))
  data6p0=filter(aset, scenario=="6p0")


  for( i in 1:length(NF)){

    NF6p0_NMXQ[i]=as.numeric(NMxQ(7, data6p0, NF[i] ))

  }


  nm7qNF6p0=data.frame(NF,NF6p0_NMXQ,rep("6p0", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("NM7Q", length(NF)), rep(sel_names[3], length(NF)))
  colnames(nm7qNF6p0)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen 8p5



  NF8p5_NMXQ=rep(0,length(NF))
  data8p5=filter(aset, scenario=="8p5")


  for( i in 1:length(NF)){

    NF8p5_NMXQ[i]=as.numeric(NMxQ(7, data8p5, NF[i] ))

  }


  nm7qNF8p5=data.frame(NF,NF8p5_NMXQ,rep("8p5", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("NM7Q", length(NF)), rep(sel_names[3], length(NF)))
  colnames(nm7qNF8p5)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )

  #### FF NMXQ
  ##### Scen 2p6

  FF2p6_NMXQ=rep(0,length(FFH))
  data2p6=filter(aset, scenario=="2p6")

  for( i in 1:length(FFH)){

    FF2p6_NMXQ[i]=NMxQ(7, data2p6, FFH[i] )

  }


  nm7qFF2p6=data.frame(FFH,FF2p6_NMXQ,rep("2p6", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("NM7Q", length(FFH)), rep(sel_names[3], length(FFH)))
  colnames(nm7qFF2p6)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen 6p0



  FF6p0_NMXQ=rep(0,length(FFH))
  data6p0=filter(aset, scenario=="6p0")


  for( i in 1:length(FFH)){

    FF6p0_NMXQ[i]=as.numeric(NMxQ(7, data6p0, FFH[i] ))

  }


  nm7qFF6p0=data.frame(FFH,FF6p0_NMXQ,rep("6p0", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("NM7Q", length(FFH)), rep(sel_names[3], length(FFH)))
  colnames(nm7qFF6p0)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen 8p5



  FF8p5_NMXQ=rep(0,length(FFH))
  data8p5=filter(aset, scenario=="8p5")


  for( i in 1:length(FFH)){

    FF8p5_NMXQ[i]=as.numeric(NMxQ(7, data8p5, FFH[i] ))

  }


  nm7qFF8p5=data.frame(FFH,FF8p5_NMXQ,rep("8p5", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("NM7Q", length(FFH)), rep(sel_names[3], length(FFH)))
  colnames(nm7qFF8p5)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )





  nm7qa3=data.frame(rbind(nm7qPast, nm7qNF2p6, nm7qNF6p0, nm7qNF8p5, nm7qFF2p6, nm7qFF6p0, nm7qFF8p5))


  #FF2=filter(dataa, period=="2.FF")



  #### Including Duration of longest Period under Q90 Threshold
  deficite=function(data, U,year){

    min=paste(as.character(year), "-11-01")
    min=sub(" -", "-",min)
    max=paste(as.character(year+1), "-10-31")
    max=sub(" -", "-",max)
    mindate=grep(min,data[,1])
    maxdate=grep(max,data[,1])
    Val=data[,2]

    datayear=data[mindate:maxdate,]

    valyear=datayear[,2]
    le=length(mindate:maxdate)







    #calculate deficite
    if(any(valyear<U)==F){return(A=0)}else{
      g=which(valyear<U)
      vals=valyear[g]
      suml=length(g)
      sum=rep(0, suml)
      for(i in 1:suml){
        sum[i]=U-vals[i]
      }
      deficite=sum(sum)
      A=deficite



    }
    return(A)
  }
  data12=data.frame(filter(aset, scenario=="hist"))[,1:2]
  Q90=quantile(data12[,2], probs=0.10)[[1]]


  #### historic dataset


  P_deficite=rep(0, length(P))


  for( i in 1:length(P)){

    #   P_deficite[i]=as.numeric(deficite(U=Q90,data=data12, year=P[i]))
    thresh=threshploty(U=Q90,data=data12, year=P[i])
    P_deficite[i]=thresh

  }


  def_P=data.frame(P,P_deficite,rep("hist", length(P)), rep("all", length(P)), rep("1.Hist", length(P)), rep("Tage < Q90", length(P)), rep(sel_names[3], length(P)))
  colnames(def_P)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  #### NF Scenario

  ##### Scen: 2p6
  NF2p6_def=rep(0,length(NF))
  data2p6=filter(aset, scenario=="2p6")


  for( i in 1:length(NF)){

    # NF2p6_def[i]=as.numeric(deficite(U=Q90,data=data2p6, year=NF[i]))
    thresh=threshploty(U=Q90,data=data2p6, year=NF[i])
    NF2p6_def[i]=thresh

  }


  def_2p6_NF=data.frame(NF,NF2p6_def,rep("2p6", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Tage < Q90", length(NF)), rep(sel_names[3], length(NF)))
  colnames( def_2p6_NF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )

  ##### Scen: 6p0
  NF6p0_def=rep(0,length(NF))
  data6p0=filter(aset, scenario=="6p0")


  for( i in 1:length(NF)){

    #NF6p0_def[i]=as.numeric(deficite(U=Q90,data=data6p0, year=NF[i]))
    thresh=threshploty(U=Q90,data=data6p0, year=NF[i])
    NF6p0_def[i]=thresh

  }


  def_6p0_NF=data.frame(NF,NF6p0_def,rep("6p0", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Tage < Q90", length(NF)), rep(sel_names[3], length(NF)))
  colnames( def_6p0_NF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen: 8p5
  NF8p5_def=rep(0,length(NF))
  data8p5=filter(aset, scenario=="8p5")


  for( i in 1:length(NF)){

    # NF8p5_def[i]=as.numeric(deficite(U=Q90,data=data8p5, year=NF[i]))
    thresh=threshploty(U=Q90,data=data8p5, year=NF[i])
    NF8p5_def[i]=thresh

  }


  def_8p5_NF=data.frame(NF,NF8p5_def,rep("8p5", length(NF)), rep("all", length(NF)), rep("2.NF", length(NF)), rep("Tage < Q90", length(NF)), rep(sel_names[3], length(NF)))
  colnames( def_8p5_NF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ### FF Scenario

  ##### Scen: 2p6
  FF2p6_def=rep(0,length(FFH))
  data2p6=filter(aset, scenario=="2p6")


  for( i in 1:length(FFH)){

    #  FF2p6_def[i]=as.numeric(deficite(U=Q90,data=data2p6, year=FFH[i]))
    thresh=threshploty(U=Q90,data=data8p5, year=NF[i])
    FF2p6_def[i]=thresh

  }


  def_2p6_FF=data.frame(FFH,FF2p6_def,rep("2p6", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("Tage < Q90", length(FFH)), rep(sel_names[3], length(FFH)))
  colnames( def_2p6_FF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )

  ##### Scen: 6p0
  FF6p0_def=rep(0,length(FFH))
  data6p0=filter(aset, scenario=="6p0")


  for( i in 1:length(FFH)){

    #   FF6p0_def[i]=as.numeric(deficite(U=Q90,data=data6p0, year=FFH[i]))
    thresh=threshploty(U=Q90,data=data6p0, year=FFH[i])
    FF6p0_def[i]=thresh


  }


  def_6p0_FF=data.frame(FFH,FF6p0_def,rep("6p0", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("Tage < Q90", length(FFH)), rep(sel_names[3], length(FFH)))
  colnames( def_6p0_FF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  ##### Scen: 8p5
  FF8p5_def=rep(0,length(FFH))
  data8p5=filter(aset, scenario=="8p5")


  for( i in 1:length(FFH)){

    #  FF8p5_def[i]=as.numeric(deficite(U=Q90,data=data8p5, year=FFH[i]))
    thresh=threshploty(U=Q90,data=data8p5, year=FFH[i])
    FF8p5_def[i]=thresh

  }


  def_8p5_FF=data.frame(FFH,FF8p5_def,rep("8p5", length(FFH)), rep("all", length(FFH)), rep("3.FF", length(FFH)), rep("Tage < Q90", length(FFH)), rep(sel_names[3], length(FFH)))
  colnames( def_8p5_FF)=c("date", "Value" ,"scenario" , "model"   ,  "period"  , "method"  , "River" )


  #### Combining deficite dataset

  defa=data.frame(rbind( def_P,def_2p6_NF, def_6p0_NF, def_8p5_NF, def_2p6_FF, def_6p0_FF, def_8p5_FF))



  defamax=which(defa$Value== max(defa$Value))
  defa3=defa[-defamax,]



  dataa3=data.frame(rbind(meanh3[,-1], nm7qa3[,-1], defa3[,-1]))



  dataa=rbind(dataa1, dataa2, dataa3)





  bp=  ggplot(data=dataa, aes(x=period, y=Value))+geom_boxplot(aes(fill=scenario), varwidth=T)+facet_grid(method~River, scales="free")+xlab("Zeitraum")+labs(title="Niedrigwasserentwicklung - Vergleich Nahe- und ferne Zukunft", subtitle="Analyse für Zuflüsse des Rheins: Main, Mosel und Neckar", fill="Szenario:")+ theme(legend.position="bottom")+ scale_fill_manual(values=c("darkorchid3", "darkolivegreen3", "skyblue1", "coral2"))

  return(bp)


}
