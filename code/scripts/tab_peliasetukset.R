output$peliAsetuksetUI<-renderUI({
  divarit<-divaridata()
  divarit<-divarit[Picked==1,.N,by=.(Divari)][order(Divari)]
  
  lapply(divarit[,Divari], function(i) {
    fluidRow(
      #column(3,textOutput(paste0("textPeliasetusDivari",i),paste0("Divari: ",i)))
      column(3,checkboxInput(paste0("checkbox_BO_mode",i),paste0("Best-of-mode päällä. Divari: ",i))),
      column(5,numericInput(paste0("numeric_rounds",i),paste0("Montako runkosarjakierrosta. Divari: ",i), value = 1)),
      column(4,numericInput(paste0("numeric_ottelut",i),paste0("Montako pelia per ottelu. Divari: ",i), value = 1))
    )
  })
  
})