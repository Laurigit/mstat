output$sarjataulukkovalitsin <- renderUI({
  required_data("ADM_PELIT")
  maxturnaus<-max(ADM_PELIT[,Turnaus_NO])
  fluidRow(numericInput("sarjataulukkokierros","Turnauksen numero",value=maxturnaus))
})

output$sarjataulukot <-renderUI({
  required_data(c("ADM_PELIT", "STG_PAKAT", "STAT_TURNAUS"))
  #input <- NULL
  #input$sarjataulukkokierros <- 29
  #input$radio_bo_mode <- TRUE
  outputdata <- UID_SARJATAULUKKO(input$sarjataulukkokierros, input$radio_bo_mode, ADM_PELIT, STG_PAKAT)
  
  kesken_turnaukset <- ADM_PELIT[is.na(Voittaja), .N, by = Turnaus_NO]
  valmiit_turnaukset <- STAT_TURNAUS[, .N, by = Turnaus_NO]
  # 
  # sarjadata<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,input$sarjataulukkokierros,input$radio_total_mode,NA,NA,NA,NA,input$radio_pfi_mode,pfi_data())
  # divarit<-sarjadata$divarit
  # pelaajat<-sarjadata$pelaajastats
  # 
  # kokonaistilanne<-pelaajat[,.(Voitot_Lauri=sum(Voitot_Lauri),Voitot_Martti=sum(Voitot_Martti))]
  kokonaistilanne <- STAT_TURNAUS[,]
  # print(kokonaistilanne)
  # tilanneteksti <-paste0(kokonaistilanne[,Voitot_Lauri],"-",kokonaistilanne[,Voitot_Martti])
  Lvoitot <- STAT_TURNAUS[Omistaja_ID == "L" & Turnaus_valmis == FALSE, Voittaja_sum]
  Mvoitot <- STAT_TURNAUS[Omistaja_ID == "M" & Turnaus_valmis == FALSE, Voittaja_sum]
   tilanneteksti <- paste0(Lvoitot,
                          "-",
                          Mvoitot)
  subtitle <- ifelse(Lvoitot > Mvoitot,"Lauri johtaa",
                    ifelse(Lvoitot < Mvoitot,"Martti johtaa","Tasan"))

  turnaustilanneteksti<-paste0(STAT_TURNAUS[Omistaja_ID == "L",sum(TurnausVoitto, na.rm = TRUE)],
                               "-",STAT_TURNAUS[Omistaja_ID == "M",sum(TurnausVoitto, na.rm = TRUE)])
  
  fluidPage(
    fluidRow(valueBox(tilanneteksti,subtitle,icon=icon("dashboard",lib = "font-awesome")),
             valueBox(turnaustilanneteksti,"Turnaustilanne",icon=icon("trophy",lib = "font-awesome"))),
    
    lapply(outputdata,function(i)  {
      plotname <- paste0("plotdyn", i, sep="")
      L_voitot <- i[Omistajaja_ID == "L", sum(Score)]
      M_voitot <- i[Omistajaja_ID == "M", sum(Score)]
      titteli <- paste0(i[, max(Divari)], ". Divari ",
                        L_voitot,
                        "-",
                        M_voitot)
      fluidRow(box(dataTableOutput(plotname),
                    width = 12,
                    title = titteli,
                    solidHeader = TRUE,
                    status = "primary"))
      
    })
  )
})
