# 
# peliData<-luecsv("pelit.csv")
# pakat<-omaReadJson("C:/Users/Lauri/Documents/R/mstat2/code/omawd/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# divariData<-luecsv("divari.csv")
# saavutusAsetukset[,minVaiMax:="max"]
# saavutusTaulu<-NULL
# saavutusTaulu<-data.table(Omistaja=character(),saavutusNimi=character(),result=numeric(),Nimi=character())
# for(kierros in 1:nrow(saavutusAsetukset)) {
#   kierrosData<-saavutusAsetukset[kierros]
#   print(kierros)
#   kierrosTulos<-laskeSaavtusAsetuksista(kierrosData,peliData,divariData,pfi_data)
#   # print(kierrosTulos)
#   saavutusTaulu<-rbind(saavutusTaulu,kierrosTulos,fill=TRUE)
# 
# }
# saavutusTaulu
# 
# tulos<-laskeSaavtusAsetuksista(saavutusKierrosAsetus,peliData,divariData,pfi_data)
laskeSaavtusAsetuksista<-function(saavutusKierrosAsetus,peliData,divariData,pfi_data){ #ui inputteja käytetään, jotta shiny server luulee että tätä päivitetään
  asetukset<-saavutusKierrosAsetus[,asetukset][[1]]
  minVaiMax<-saavutusKierrosAsetus[,minVaiMax]
  minVaiMax_rivi<-saavutusKierrosAsetus[,minVaiMax_rivi]
  lahtoData<-saavutusKierrosAsetus[,datataulu]
  saavutusNimi<-saavutusKierrosAsetus[,kuvaus]
  Palkintonimi<-saavutusKierrosAsetus[,Palkintonimi]
  Esitysmuoto<-saavutusKierrosAsetus[,Esitysmuoto]
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
  if (nrow(valittuData)>0) {
  
  
  omaCountUnique<-function(inData){
  tulos<-length(unique(inData))
  return(tulos)
  }
  
  #jos cols on tyhjä, niin lisää dummy
  if(is.null(cols)){
    valittuData[,dummy:="Dummy"]
    cols<-"dummy"
  }
  pivotDataOut<-as.data.table(dcast(data=valittuData,
                                    formula= as.formula(paste(paste(rows, collapse="+"), "~" ,paste(cols,collapse="+"))),
                                                        value.var=vals,fun.aggregate=get(aggt_to_dcast)))
  #remove Nmi and Omistaja
  getColsNames<-names(pivotDataOut)
  remove<- c(vals,cols,rows,"dummy")
  numeric_cols<-getColsNames[!getColsNames %in% remove]
  
  
  #rivimaksimit ja minimit
  pivotDataOut[, `:=`(
                    min = min(.SD, na.rm=TRUE),
                    max = max(.SD, na.rm=TRUE)),.SDcols=numeric_cols,by=1:nrow(pivotDataOut)] 
  pivotDataOut[,riviTulos:=get(minVaiMax_rivi)]

  if(minVaiMax=="max") {
  Paras<-pivotDataOut[which.max(riviTulos)][,source:="Paras"]
  Huonoin<- pivotDataOut[which.min(riviTulos)][,source:="Huonoin"]
  Keskiarvo<-pivotDataOut[,mean(riviTulos)]
  } else {
  Paras<-pivotDataOut[which.min(riviTulos)][,source:="Paras"]
  Huonoin<- pivotDataOut[which.max(riviTulos)][,source:="Huonoin"]
  Keskiarvo<-pivotDataOut[,mean(riviTulos)]
  }
  append_PH<-rbind(Paras,Huonoin)
  append_PH[,Keskiarvo:=Keskiarvo]
  append_PH[ ,':='(min= NULL,max=NULL,result=riviTulos)]
  append_PH[,c(numeric_cols):=NULL]
  #join kuvaus
  kuvaus<-cbind(append_PH,saavutusNimi)
  #omistaja kasittely
  kuvaus[,color:=ifelse(Omistaja=="Martti","yellow","purple")]
  
  #konvertoi resultin esitysmuoto
  convEsitys<-function(Esitysmuoto,inputNumero){
    if(Esitysmuoto=="%") {
      tulos <-paste0(round(inputNumero,2)*100,"%")
    } else if (Esitysmuoto=="Decimal") {
      tulos<-round(inputNumero,2)
    } else if(Esitysmuoto=="€") {
      tulos<-paste(round(inputNumero,0),"€")
    } else if (Esitysmuoto=="Integer") {
      tulos<-round(inputNumero,0)
    }
    return((tulos))
   
  }
  #korjaa esitysmuoto
  kuvaus[,':=' (txtResult=convEsitys(Esitysmuoto,result),txtKa=convEsitys(Esitysmuoto,Keskiarvo))]
  
  if("Nimi" %in% colnames(kuvaus)) {
    kuvaus[,teksti:=paste0("<h4><i>",Palkintonimi,"-Palkinto: </i><b><br/>", Nimi,"</b><br/>",
                           saavutusNimi,": <b>",txtResult," </b> <h4/>", "<br/>",
                           "Keskiarvo: ", txtKa)]
  } else {
    kuvaus[,teksti:=paste0("<h4><i>",Palkintonimi,"-Palkinto: </i><br/>", Omistaja, "<br/>",
                           saavutusNimi,": <b>",txtResult,"</b><h4/>")]
  }
print(kuvaus)

return(kuvaus)
  } else {
  return(NULL)
}
}

