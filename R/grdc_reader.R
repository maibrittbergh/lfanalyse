#'GRDC-Data List
#'
#'@description Function returns list. It may take some time to load the GRDC-List, depending on the amount of stations within the metadataset.
#'
#'
#' @param metadata Data Frame; Overview of GRDC-Dataset.  The metadata can be created by \link[dischanalyst]{metadata_grdc} function.
#' @param path character; Pathway to local grdc data folder.
#'
#' @return Grdc List. Each entry contains discharge measurements as well as the corresponding date of a specific station. The number of the displayed stations depends on the rownumber of the metadataset.
#' @export
#'
#' @examples
#'\dontrun{
#' grdc_list(metadata_germany, "/Users/username/Desktop/folderone/datafolder/grdc_03_2021/grdc_disc/")
#'}
#'
#'
grdc_reader=function(metadata, path){

  single_reader=function(metadata, rivername, path ){
    data_fluss= metadata[which(metadata$river==rivername),]
    grdc_no=data_fluss$grdc_no
    l=length(grdc_no)
    read=c(1:l)
    for (i in 1:l){

      grdc_numb=grdc_no[i]
      grdc_numb=as.character(grdc_numb)
      read[i]=paste(path,"/",grdc_numb,"_Q_Day.Cmd.txt")
    }
    read=sub(" ","", read)
    read=sub(" _","_", read)
    read=sub("/ ", "/", read)
    name= vector(mode = "list", length = l)

    for (i in 1:l){
      Tabelle=read.table(read[i], header=T, sep=";", dec=".", na.strings = "NA")[-2]#-999 als NA Value
      Tabelle$YYYY.MM.DD=as.Date(Tabelle$YYYY.MM.DD)
      Tabelle$Value[(which(Tabelle$Value<0))] = NA
      name[[i]]=Tabelle #hours,minutes rausgeschmissen
    }

    for (i in 1:l){
      names(name)[[i]]= data_fluss$station[i]
    }

    return(name)
  }

  length=nrow(metadata)
  grdc_list=vector(mode = "list", length = length)



  for ( i in 1:length){

    data=single_reader(metadata,   metadata$river[i]    , path)
    station=metadata$station[i]
    nbr=which(names(data)== station)
    val=data[[nbr]]



    grdc_list[[i]]=val
    names(grdc_list)=metadata$station

  }


  return(grdc_list)
}
