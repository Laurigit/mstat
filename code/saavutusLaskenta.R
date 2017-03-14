
peliData<-luecsv("pelit.csv")
pakat<-omaReadJson("C:/Users/Lauri/Documents/R/mstat2/code/omawd/")
pfi_data<-pakkaUutuusProsentti(pakat)
divariData<-luecsv("divari.csv")
saavutusTaulu<-NULL
saavutusTaulu<-data.table(Omistaja=character(),saavutusNimi=character(),result=numeric(),Nimi=character())
for(kierros in 1:nrow(saavutusAsetukset)) {
  kierrosData<-saavutusAsetukset[kierros]

  kierrosTulos<-laskeSaavtusAsetuksista(kierrosData,peliData,divariData,pfi_data)
  print(kierrosTulos)
  saavutusTaulu<-rbind(saavutusTaulu,kierrosTulos,fill=TRUE)

}
saavutusTaulu
tulostaulu<-laskeSaavtusAsetuksista()

laskeSaavtusAsetuksista<-function(saavutusKierrosAsetus,peliData,divariData,pfi_data){
  asetukset<-saavutusKierrosAsetus[,asetukset][[1]]
  minVaiMax<-saavutusKierrosAsetus[,minVaiMax]
  lahtoData<-saavutusKierrosAsetus[,datataulu]
  saavutusNimi<-saavutusKierrosAsetus[,kuvaus]
  cols<-unlist(asetukset[[1]])
  rows<-unlist(asetukset[[2]])
  vals<-asetukset[[3]][[1]]
  filters<-asetukset[[4]]
  filter_col_names<-names(filters)
  aggr<-asetukset[[5]]
  #conver aggr to proper format
  conversioTaulu<-data.table(pivot=c("Count",
                                   "Count Unique Values",
                                   "Sum",
                                   "Average",
                                   "Minimum",
                                   "Maximum"),
                           dcast=c("length","omaCountUnique","sum", "mean","min","max"))
  
  aggt_to_dcast<-conversioTaulu[pivot==aggr,dcast]
  pivotData<-tilastoMurskain(divariData,peliData,pfi_data,input_bo_mode=FALSE,input_moving_average=5,input_pfiMA=NA)
  if(lahtoData=="Aikasarja") {
  outputData<-pivotData$aikasarja
  } else if (lahtoData=="Ristidata"){
  outputData<-pivotData$cross
  } else {
  outputData<-pivotData$turnaus
  }
  valittuData<-outputData[,c(vals,cols,rows,filter_col_names),with=FALSE]
  
  #filtteröi valittu data
  if(length(filters)>0){
  for(kierros in 1:length(filters)) {
  kierrosData<-filters[[kierros]]
  syntax_start<-parse(text=paste0('!',names(filters)[[kierros]],' %in% c("',paste(unlist(kierrosData),collapse='","'),'")'))
  #
  valittuData<-valittuData[eval(syntax_start)]
  }
  }
  
  
  omaCountUnique<-function(inData){
  tulos<-length(unique(inData))
  return(tulos)
  }
  
  #jos cols on tyhjä, niin lisää dummy
  if(is.null(cols)){
    valittuData[,dummy:="Dummy"]
    cols<-"dummy"
  }
  pivotDataOut<-as.data.table(dcast(data=valittuData,formula= as.formula(paste(paste(rows, collapse="+"), "~" ,cols)),value.var=vals,fun.aggregate=get(aggt_to_dcast)))
  #remove Nmi and Omistaja
  getColsNames<-names(pivotDataOut)
  remove<- c(vals,cols,rows,"dummy")
  numeric_cols<-getColsNames[!getColsNames %in% remove]
  
  
  
  pivotDataOut[, `:=`(
                    min = min(.SD, na.rm=TRUE),
                    max = max(.SD, na.rm=TRUE)),.SDcols=numeric_cols,by=1:nrow(pivotDataOut)] 
  
  print(pivotDataOut)
  if(minVaiMax=="max") {
  Paras<-pivotDataOut[which.max(max)][,source:="Paras"]
  Huonoin<- pivotDataOut[which.min(max)][,source:="Huonoin"]
  Keskiarvo<-pivotDataOut[,mean(max)]
  } else {
  Paras<-pivotDataOut[which.min(min)][,source:="Paras"]
  Huonoin<- pivotDataOut[which.max(min)][,source:="Huonoin"]
  Keskiarvo<-pivotDataOut[,mean(min)]
  }
  append_PH<-rbind(Paras,Huonoin)
  append_PH[,Keskiarvo:=Keskiarvo]
  append_PH[ ,':='(min= NULL,max=NULL,result=get(minVaiMax))]
  append_PH[,c(numeric_cols):=NULL]
  #join kuvaus
  result<-cbind(append_PH,saavutusNimi)
return(result)
}

