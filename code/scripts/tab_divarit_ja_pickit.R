# output$table_divari2<- renderUI({
#   #montakodivaria
#   print("montako divaria alku")
#   divarit<-divaridata()
#   divarit<-divarit[order(Divari)]
#   eri_divarit<-unique(divarit[,Divari])
#   
#   fluidPage(
#     lapply(eri_divarit,function(i)  {
#       plotname2 <- paste0("plotdyndivari", i, sep="")
#       
#       fluidRow(box( dataTableOutput(plotname2),width=12,title=paste0("Divari: ",i),solidHeader = TRUE,status="primary" ))
#       
#     })
#   )
#   
# })

eR_UID_DIVARI <- reactive({
  refresh_counter$a
  required_data(c("STG_DIVARI", "STG_PAKAT", "STAT_LISAKORTIT", "ADM_PELIT"))
  results <- UID_DIVARI(STG_DIVARI, STG_PAKAT, STAT_LISAKORTIT, ADM_PELIT)
  return(results)
}) 


# for (i in 0:10) {
#   
#   # Need local so that each item gets its own number. Without it, the value
#   # of i in the renderPlot() will be the same across all instances, because
#   # of when the expression is evaluated.
#   local({
#     my_i <- i
# 
#     plotname_divari <- paste0("plotdyndivari", my_i, sep="")
#     output[[plotname_divari]] <- renderDataTable({
#       divarit<-eR_UID_DIVARI()
#       tempdata<-divarit[Picked==1]
#       # Data<-tempdata[Divari==my_i]
#       Data<-tempdata[Divari==my_i]
#       DataOut <- Data[, .(Omistaja_ID, Pakka_NM, Score, Kortit = Lisakortit_lkm)]
#       return(DataOut)
#       #print(Data)
#     },    options = list(
#       paging = FALSE,
#       searching = FALSE,
#       info=FALSE
#       
#     )
#     )
#   })
# }

eR_UID_PICKIT <- reactive({
  refresh_counter$a
  print("eR_UID_PICKIT")
  required_data(c("STG_DIVARI", "STG_PAKAT"))
  result <- UID_PICKIT(STG_DIVARI, STG_PAKAT)
  return(result)
})

#divariNumericinput
output$combUI<-renderUI({
  message("output$combUI")
  divarit<-eR_UID_PICKIT()
  # required_data(c("STG_DIVARI", "STG_PAKAT"))
  # divarit <- UID_PICKIT(STG_DIVARI, STG_PAKAT)
  message(divarit)
  ranking <- eR_UID_DIVARI()
  message(ranking)
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
              input_label <- paste0(pakkanimi, ": ", Score, " Cards: ", Kortit)
              
             
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
