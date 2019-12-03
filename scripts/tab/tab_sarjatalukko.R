
for (i in 1:10) {
local({
  my_i <- i
  plotname <- paste0("plotdyn", my_i, sep="")
  

  output[[plotname]] <- renderDataTable({
    required_data(c("ADM_PELIT", "STG_PAKAT"))
   # input <- NULL
   # input$sarjataulukkokierros <- 27
   # input$radio_bo_mode <- TRUE
   # Data <- UID_SARJATAULUKKO(input$sarjataulukkokierros, input$radio_bo_mode, ADM_PELIT, STG_PAKAT)
   # print("LOOP AT TAB_SARJATAULUKKO")
    Data <-  eR_UID_SARJATAULUKKO()
   # print(eR_UID_SARJATAULUKKO())
 
  
    DivariData <- Data[[my_i]]
    #print(DivariData)
    if(!is.null(DivariData)) {
    outputData <- DivariData[, .(Pakka_NM,
                                 Matches,
                                 Wins,
                                 Draws,
                                 Losses,
                                 Score)]
    } else {
      outputData <- data.table(not_played = "-")
    }
    return(outputData)
    #print(Data)
  },    options = list(
    paging = FALSE,
    searching = FALSE,
    info = FALSE

    
  ),
  rownames= FALSE
  )
})
}


output$sarjataulukkovalitsin <- renderUI({
  required_data("ADM_PELIT")
  #print("output$sarjataulukkovalitsin")
  maxturnaus <- max(ADM_PELIT[,Turnaus_NO])
  message(maxturnaus, "maxturnaus")
  fluidRow(column(4, numericInput("sarjataulukkokierros",
                        "Turnauksen numero",
                        value = maxturnaus)),
           column(width = 4,
                  
                  radioButtons("radio_total_mode",
                        label = ("Total mode"),
                        choices = list("Pois" = FALSE, "Paalla" = TRUE),
                        selected = FALSE,
                        inline = T))
           )
})

eR_UID_SARJATAULUKKO <- reactive({
#print("eR_UID_SARJATAULUKKO")
  message(input$sarjataulukkokierros," input$sarjataulukkokierros")
  #take dep
  refresh_counter$a 
  ###
  result <-  UID_SARJATAULUKKO(input$sarjataulukkokierros, input$radio_bo_mode, ADM_PELIT, STG_PAKAT,
                               input$radio_total_mode)
  return(result)
})

output$sarjataulukot <-renderUI({
  required_data(c("ADM_PELIT", "STG_PAKAT", "STAT_TURNAUS", "STAT_RUNNING_TURNAUS"))
  #print("output$sarjataulukot")
  #input <- NULL
  #input$sarjataulukkokierros <- 27
  #input$radio_bo_mode <- FALSE
  #sarjataulukkoData <- UID_SARJATAULUKKO(input$sarjataulukkokierros, input$radio_bo_mode, ADM_PELIT, STG_PAKAT)
  sarjataulukkoData <- eR_UID_SARJATAULUKKO()
  #total <- rbindlist(sarjataulukkoData, use.names = TRUE)

  total <- do.call(rbind, sarjataulukkoData)

  # 
  # sarjadata<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,input$sarjataulukkokierros,input$radio_total_mode,NA,NA,NA,NA,input$radio_pfi_mode,pfi_data())
  # divarit<-sarjadata$divarit
  # pelaajat<-sarjadata$pelaajastats
  # 
  # kokonaistilanne<-pelaajat[,.(Voitot_Lauri=sum(Voitot_Lauri),Voitot_Martti=sum(Voitot_Martti))]

  # print(kokonaistilanne)
  # tilanneteksti <-paste0(kokonaistilanne[,Voitot_Lauri],"-",kokonaistilanne[,Voitot_Martti])
  #current_turnaus <- STAT_TURNAUS[Turnaus_valmis == FALSE]
  Lvoitot <- total[Omistaja_ID == "L", sum(Score)]
  Mvoitot <- total[Omistaja_ID == "M" , sum(Score)]
   tilanneteksti <- paste0(Lvoitot,
                          "-",
                          Mvoitot)
  subtitle <- ifelse(Lvoitot > Mvoitot,"Lauri johtaa",
                    ifelse(Lvoitot < Mvoitot,"Martti johtaa","Tasan"))

  turnaustilanneteksti <- paste0(STAT_TURNAUS[Omistaja_ID == "L",sum(TurnausVoitto, na.rm = TRUE)],
                               "-",STAT_TURNAUS[Omistaja_ID == "M",sum(TurnausVoitto, na.rm = TRUE)])
  running_teksti <- paste0(STAT_RUNNING_TURNAUS[Turnaus_Valmis == 0, running_voitot_L],
                           "-",
                           STAT_RUNNING_TURNAUS[Turnaus_Valmis == 0, running_voitot_M])
  
  running_teksti_total <- paste0(STAT_RUNNING_TURNAUS[Turnaus_Valmis == 1, max(running_turnaus_voitot_L)],
                                "-",
                                STAT_RUNNING_TURNAUS[Turnaus_Valmis == 1, max(running_turnaus_voitot_M)]) 
  
  Divarit <- sort(total[Omistaja_ID == "L", unique(Divari)])
  
  fluidPage(
    fluidRow(#column(3,
                    valueBox(width = 3, tilanneteksti,subtitle,icon=icon("dashboard",lib = "font-awesome")),
           #  column(3,
                    valueBox(width = 3, turnaustilanneteksti,"Turnaustilanne",icon=icon("trophy",lib = "font-awesome")),
           #  column(3,
                    valueBox(width = 3, running_teksti, "Trophy competition", icon = icon("dashboard", lib = "font-awesome"), color = "blue"),
          #   column(3,
                    valueBox(width = 3, running_teksti_total, "Trophy count", icon = icon("trophy", lib = "font-awesome"), color = "blue")
    ),
    
    lapply(Divarit,function(i)  {
      plotname <- paste0("plotdyn", i, sep="")
      L_voitot <- sarjataulukkoData[[i]][Omistaja_ID == "L", sum(Score)]
      M_voitot <- sarjataulukkoData[[i]][Omistaja_ID == "M", sum(Score)]
      titteli <- paste0(sarjataulukkoData[[i]][, max(Divari)], ". Divari ",
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


