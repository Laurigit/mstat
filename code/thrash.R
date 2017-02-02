sarjataulukkoKaikki(FALSE,1,TRUE,NA,3,1,3)









#tee divaritaulut2
divaridata <- reactiveFileReader(1000, session, "divari.csv",luecsv)
output$table_divari2<- renderDataTable(
  { 
    
    divarit<-divaridata()
    divarit<-divarit[order(Divari)]
    Data<-divarit[Picked==1,.(Omistaja=Omistaja_nimi,Nimi,Divari,Picked)]
  },
  options = list(
    paging = FALSE,
    searching = FALSE,
    info=FALSE,
    rowCallback = I(
      'function(row, data) {
      if (data[2] % 2 == "1" )
      $("td", row).css("background", "orange");}'
    )
    )
    )




,
tabItem(tabName="tab_sarjataulukko_all"#,
        
        #fluidRow(numericInput("sarjataulukkokierros","Valitse turnauksen numero",value=1)),
        #fluidRow(dataTableOutput("sarjataulukko")),
        
        # fluidRow(box(DT::dataTableOutput("sarjataulukot_all"),width=12,title="Kaikki pelit", solidHeader = TRUE,status="primary"))
        
        
        
        
) 

menuItem("Sarjataulukko yhteensa",tabName="tab_sarjataulukko_all",icon=icon("th")),