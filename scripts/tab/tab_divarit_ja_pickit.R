
#divariNumericinput
output$combUI<-renderUI({
  refresh_counter$a
  message("output$combUI")
  required_data("ADM_PICKIT")
  divarit<-ADM_PICKIT
  # required_data(c("STG_DIVARI", "STG_PAKAT"))
  # divarit <- UID_PICKIT(STG_DIVARI, STG_PAKAT)

  required_data(c("ADM_DIVARI", "ADM_PELIT"))
  ranking <- ADM_DIVARI
  edellinen_divari <- ADM_PELIT[ADM_PELIT[, .I[which.max(Turnaus_NO)], by=Pakka_ID]$V1][, .(Pakka_ID, Divari)]

 
  #required_data(c("STG_DIVARI", "STG_PAKAT", "STAT_LISAKORTIT", "ADM_PELIT"))
  
  #ranking <- UID_DIVARI(STG_DIVARI, STG_PAKAT , STAT_LISAKORTIT, ADM_PELIT)
  setorder(ranking, Divari, -Score, -Lisakortit_lkm)
  ranking_ss <- ranking[, .(Score, Lisakortit_lkm, Pakka_ID, rank = seq_len(.N))]
  joinrank <- ranking_ss[divarit, on = "Pakka_ID"]
  setorder(joinrank, rank)
  divarilist <- joinrank[, .N, by = .(Divari, Picked)]
  #divarit <- UID_PICKIT(STG_DIVARI, STG_PAKAT)
  setorder(divarilist, -Picked, Divari)
  divarilist[, rivi := seq_len(.N)]
  fluidPage(
  lapply(divarilist[,rivi], function(i) {
   
    loop_divari <- divarilist[i, Divari]
    loop_picked <- divarilist[i, Picked]
  

    fluidRow(
      # box(
      #   dataTableOutput(plotname_divari)),
    
            lapply(sort(joinrank[Divari == loop_divari & Picked == loop_picked, unique(Omistaja_ID)]), function(k) {
              loop_omistaja <- k
              title_text <- paste0("Div: ", loop_divari, " Picked: ", loop_picked)
              

              box(width = 6,
                  title = title_text,
                 
              lapply(joinrank[Divari == loop_divari & Picked == loop_picked & Omistaja_ID == loop_omistaja, Pakka_ID], function(j){
              pakkanimi <- joinrank[Pakka_ID == j, Pakka_NM]
              Omistaja <- joinrank[Pakka_ID == j, Omistaja_ID]
              Score <- joinrank[Pakka_ID == j, Score]
              Kortit <- joinrank[Pakka_ID == j, Lisakortit_lkm]
              Old_divari <- edellinen_divari[Pakka_ID == j, Divari]
              input_label <- paste0(pakkanimi, ": ", Score, " Cards: ", Kortit, "Old_Div :", Old_divari)
              
             
              fluidRow(        
                column(8,
                     numericInput(paste0("nimput", divarit[Pakka_ID == j, Pakka_ID]),
                                  label = input_label,
                                  value = divarit[Pakka_ID == j, Divari])),
                column(4,
                     checkboxInput(paste0("checkbox", divarit[Pakka_ID == j, Pakka_ID]),
                                   label = substr(paste0(divarit[Pakka_ID == j, Pakka_NM]), 1, 3),
                                   value = divarit[Pakka_ID == j, Picked])))
              
              })
              )
              })
            
          )
  })
  )
  
})

#paivitä bannit
observeEvent(input$tallenna_bannit,{
  shinyjs::disable("tallenna_bannit")
  
  print("tallenna bannit alku")
  required_data("STG_DIVARI")
  divarit<-STG_DIVARI
  divarit[,syntax_cb:=(text=paste0("checkbox",Pakka_ID))]
  divarit[,syntax:=(text=paste0("nimput", Pakka_ID))]
  
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

    shinyjs::enable("luo_peleja")

  print(divarit)
  raakadivari <- luecsv("divari.csv")
  #poista vanhat arvot
  raakadivari[, ':=' (Divari = NULL,
                      Picked = NULL)]
  #joinaa uudet
  joinuus <- divarit[raakadivari, on = .(Pakka_ID == rivi_id)]
  setnames(joinuus, "Pakka_ID", "rivi_id")
  kircsv(joinuus, "divari.csv", upload = TRUE)
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_DIVARI", ADM_DI_HIERARKIA, input_env = globalenv())
  refresh_counter$a <-   refresh_counter$a +1
  print("tallenna bannit loppu")
  shinyjs::enable("tallenna_bannit")
})
