output$sarjataulukkovalitsin <- renderUI({
  maxturnaus<-max(peliDataReact()[,TurnausNo])
  fluidRow(numericInput("sarjataulukkokierros","Turnauksen numero",value=maxturnaus))
})

output$sarjataulukot <-renderUI({
  #montakodivaria
  sarjadata<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,input$sarjataulukkokierros,input$radio_total_mode,NA,NA,NA,NA,input$radio_pfi_mode,pfi_data())
  divarit<-sarjadata$divarit
  pelaajat<-sarjadata$pelaajastats
  
  kokonaistilanne<-pelaajat[,.(Voitot_Lauri=sum(Voitot_Lauri),Voitot_Martti=sum(Voitot_Martti))]
  print(kokonaistilanne)
  tilanneteksti <-paste0(kokonaistilanne[,Voitot_Lauri],"-",kokonaistilanne[,Voitot_Martti])
  subtitle<-ifelse(kokonaistilanne[,Voitot_Lauri]>kokonaistilanne[,Voitot_Martti],"Lauri johtaa",
                   ifelse(kokonaistilanne[,Voitot_Lauri]<kokonaistilanne[,Voitot_Martti],"Martti johtaa","Tasan"))
  turnaustilanne<-turnausVoitot(divaridata(),peliDataReact())$total
  turnaustilanneteksti<-paste0(turnaustilanne[,Laurin_TV],"-",turnaustilanne[,Martin_TV])
  
  fluidPage(
    fluidRow(valueBox(tilanneteksti,subtitle,icon=icon("dashboard",lib = "font-awesome")),
             valueBox(turnaustilanneteksti,"Turnaustilanne",icon=icon("trophy",lib = "font-awesome"))),
    
    lapply(divarit,function(i)  {
      plotname <- paste0("plotdyn", i, sep="")
      
      fluidRow(box( dataTableOutput(plotname),width=12,title=paste0(i,". Divari ",pelaajat[Divari==i,Voitot_Lauri],"-",pelaajat[Divari==i,Voitot_Martti]),solidHeader = TRUE,status="primary" ))
      
    })
  )
})