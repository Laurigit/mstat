# 
# peliData<-luecsv("pelit.csv")
# pakat<-omaReadJson("./external_files/")
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

# tulos<-laskeSaavtusAsetuksista(saavutusKierrosAsetus,peliData,divariData,pfi_data)
laskeSaavtusAsetuksista<-function(saavutusKierrosAsetus, saavutusDataInput){ #ui inputteja käytetään, jotta shiny server luulee että tätä päivitetään
 required_data("saavutusAsetukset")
  saavutusKierrosAsetus <- saavutusDataInput[saavutusKierrosAsetus]
  saavutusKierrosAsetus[, asetukset]
  asetukset<-saavutusKierrosAsetus[,asetukset][[1]]
  minVaiMax<-saavutusKierrosAsetus[,minVaiMax]
  minVaiMax_rivi<-saavutusKierrosAsetus[,minVaiMax_rivi]
  lahtoData<-saavutusKierrosAsetus[,datataulu]
  saavutusNimi<-saavutusKierrosAsetus[,kuvaus]
  Palkintonimi<-saavutusKierrosAsetus[,Palkintonimi]
  Esitysmuoto<-saavutusKierrosAsetus[,Esitysmuoto]
  cols<-unlist(asetukset[[1]])
  rows<-unlist(asetukset[[2]])
  
  #jos valittuna on coutn, niin vals ei saa arvoa.
  vals = tryCatch({
    vals<-asetukset[[3]][[1]]
  
  }, error = function(e) {
    vals <- NULL
  })
  

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
 
  required_data(lahtoData)
  required_data("STG_PAKAT")
  outputData <- get(lahtoData)
  selectCols <- unique(c(vals,cols,rows,filter_col_names))
  valittuData<-outputData[,selectCols,with=FALSE]
  
  #filtteröi valittu data
  if(length(filters)>0){
  for(kierros in 1:length(filters)) {
  kierrosData<-filters[[kierros]]
  syntax_start<-parse(text=paste0('!',names(filters)[[kierros]],' %in% c("',paste(unlist(kierrosData),collapse='","'),'")'))
  #
  valittuData<-valittuData[eval(syntax_start)]
 #jos null, niin vaadi finite
  if (unlist(kierrosData) == "null") {
    syntax_NA <- parse(text=paste0('!is.na(',names(filters)[[kierros]],')'))
    valittuData <- valittuData[eval(syntax_NA)]
  }
  
  
  }
  }
  if (nrow(valittuData)>0) {
            
            
            omaCountUnique <- function(inData){
            tulos<-length(unique(inData))
            return(tulos)
            }
            
            #jos cols on tyhjä, niin lisää dummy
            if(is.null(cols)){
              valittuData[,dummy:="Dummy"]
              cols<-"dummy"
            }
            if(is.null(vals)){
              valittuData[,dummy:="Dummy"]
              vals<-"dummy"
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
            #omistaja kasittely. joinaa se ja pakkanimi
            ssColsPakat <- STG_PAKAT[, .(Pakka_ID, Omistaja_ID, Pakka_NM)]
            setnames(ssColsPakat, "Pakka_ID", rows)
            
            joinOmistaja <- ssColsPakat[kuvaus, on = rows]
            joinOmistaja[,color:=ifelse(Omistaja_ID=="M","yellow","purple")]
            
          
            #korjaa esitysmuoto
            joinOmistaja[,':=' (txtResult=convEsitys(Esitysmuoto,result),txtKa=convEsitys(Esitysmuoto,Keskiarvo))]
            
            joinOmistaja[,teksti:=paste0("<h4><i>",Palkintonimi,"-Palkinto: </i><b><br/>", Pakka_NM ,"</b><br/>",
                                                                   saavutusNimi,": <b>",txtResult," </b> <h4/>", "<br/>",
                                                                   "Keskiarvo: ", txtKa)]
            # 
            # if("Nimi" %in% colnames(joinOmistaja)) {
            #   joinOmistaja[,teksti:=paste0("<h4><i>",Palkintonimi,"-Palkinto: </i><b><br/>", Nimi,"</b><br/>",
            #                          saavutusNimi,": <b>",txtResult," </b> <h4/>", "<br/>",
            #                          "Keskiarvo: ", txtKa)]
            # } else {
            #   joinOmistaja[,teksti:=paste0("<h4><i>",Palkintonimi,"-Palkinto: </i><br/>", Omistaja, "<br/>",
            #                          saavutusNimi,": <b>",txtResult,"</b><h4/>")]
            # }
          print(joinOmistaja)
          
          return(joinOmistaja)
            } else {
  return(NULL)
}
}

#konvertoi resultin esitysmuoto
convEsitys<-function(Esitysmuoto,inputNumero){
  if(Esitysmuoto=="%") {
    tulos <-paste0(round(inputNumero,2)*100,"%")
  } else if (Esitysmuoto=="Decimal") {
    tulos<-round(inputNumero,2)
  } else if(Esitysmuoto=="e") {
    tulos<-paste(round(inputNumero,0),"e")
  } else if (Esitysmuoto=="Integer") {
    tulos<-round(inputNumero,0)
  }
  return((tulos))
  
}
