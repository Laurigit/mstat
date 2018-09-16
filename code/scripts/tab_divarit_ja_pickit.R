output$table_divari2<- renderUI({
  #montakodivaria
  print("montako divaria alku")
  divarit<-divaridata()
  divarit<-divarit[order(Divari)]
  eri_divarit<-unique(divarit[,Divari])
  
  fluidPage(
    lapply(eri_divarit,function(i)  {
      plotname2 <- paste0("plotdyndivari", i, sep="")
      
      fluidRow(box( dataTableOutput(plotname2),width=12,title=paste0("Divari: ",i),solidHeader = TRUE,status="primary" ))
      
    })
  )
  
})



for (i in 0:10) {
  
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    my_i <- i
    # plotname <- paste0("plotdyn", my_i, sep="")
    # output[[plotname]] <- renderDataTable({
    #   Data_all<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,input$sarjataulukkokierros,input$radio_total_mode,my_i,NA,NA,NA,input$radio_pfi_mode,pfi_data())$sarjataulukko
    #   Data<-Data_all
    #   return(Data)
    #   #print(Data)
    # },    options = list(
    #   paging = FALSE,
    #   searching = FALSE,
    #   info=FALSE
    #   
    # )
    # )
    plotname_divari <- paste0("plotdyndivari", my_i, sep="")
    output[[plotname_divari]] <- renderDataTable({
      divarit<-divaridata()
      tempdata<-divarit[Picked==1,.(Omistaja=Omistaja_nimi,Nimi,Divari,Picked)]
      # Data<-tempdata[Divari==my_i]
      Data<-tempdata[Divari==my_i]
      return(Data)
      #print(Data)
    },    options = list(
      paging = FALSE,
      searching = FALSE,
      info=FALSE
      
    )
    )
  })
}


#divariNumericinput
output$combUI<-renderUI({
  divarit<-divaridata()
  
  lapply(divarit[,rivi_id], function(i) {
    fluidRow(
      column(3,  numericInput(paste0( divarit[rivi_id==i,Pakka],divarit[rivi_id==i,Omistaja]), label=paste0(divarit[rivi_id==i,Nimi]),value=divarit[rivi_id==i,Divari])),
      column(3,h4("Divari/picked")),
      column(3, checkboxInput(paste0("checkbox", divarit[rivi_id==i,Pakka],divarit[rivi_id==i,Omistaja]),label=paste0(divarit[rivi_id==i,Nimi]),value=divarit[rivi_id==i,Picked])))
    
    
  })
  
})

#paivitä bannit
observeEvent(input$tallenna_bannit,{
  print("tallenna bannit alku")
  
  divarit<-divaridata()
  divarit[,syntax_cb:=(text=paste0("checkbox",Pakka,Omistaja))]
  divarit[,syntax:=(text=paste0(Pakka,Omistaja))]
  
  lapply(divarit[,syntax_cb],function(i) {
    
    divarit[syntax_cb==i,Picked:=as.numeric((input[[i]]))]
    
  })
  lapply(divarit[,syntax],function(i) {
    
    divarit[syntax==i,Divari:=input[[i]]]
    
    
  })
  
  divarit[,syntax:=NULL]
  divarit[,syntax_cb:=NULL]
  
  #validoi divari
  #laske montako pakkaa per divari
  pakkavalidoi<-divarit[,.N,by=.(Omistaja,Divari)]
  #laske montako omistajaa per divari
  omistajavalidoi<-pakkavalidoi[,.N,by=.(Divari)]
  if(min(omistajavalidoi[,N]==2)){
    shinyjs::enable("luo_peleja")
  } else {
    shinyjs::disable("luo_peleja")
  }
  kircsv(divarit,"divari.csv", upload = TRUE)
  
  print("tallenna bannit loppu")
})
