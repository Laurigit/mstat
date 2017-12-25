aikaero<-function(aika,loppuaika,pvm,loppupvm){
  return(((loppupvm-pvm)*60*60*24+loppuaika-aika))
  
}


#oma_timedate
oma_timedate<-function(pvm,aika) {
  tulos<-as.integer(pvm)*24*60*60+as.integer(aika)
  return(tulos)
  
}

kategorisoi<-function(arvoVektori,kategorisointiVektori=NULL,pct_vektori=c(0.2, 0.4, 0.6, 0.8)) {
  if(is.null(kategorisointiVektori)) {
    kategorisointiVektori<-arvoVektori
  }
  kvantiilit <-quantile(kategorisointiVektori,pct_vektori)
  #laske, että niitä on 4 erilaista
  eriKvant<-unique(kvantiilit)
  if(length(eriKvant)==4) {
    #lisää minimi ja maksimi
    minHavainto<-min(kategorisointiVektori)
    maxHavainto<-max(kategorisointiVektori)
    tulos<-cut(arvoVektori,breaks=sort(unique(c(minHavainto,as.numeric(kvantiilit),maxHavainto))),include.lowest=TRUE)
    return (tulos)
    
  } else {
    varaTulos<-cut(arvoVektori,sort(unique(kategorisointiVektori)),include.lowest=TRUE)
    return(varaTulos)
  }
}

list_to_string <- function(obj, listname) {
  if (is.null(names(obj))) {
    paste(listname, "[[", seq_along(obj), "]] = ", obj,
          sep = "", collapse = "\n")
  } else {
    paste(listname, "$", names(obj), " = ", obj,
          sep = "", collapse = "\n")
  }
}
