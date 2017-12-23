#arvopeli
observeEvent(input$arvo_peli,{
  print("arvo peli alku")
  kaikkipelit<-peliDataReact()
  #kato onko divarifiltteri päällä
  if(input$divariRadio!="Ei väliä") {
    pelaamattomat <- unique(kaikkipelit[is.na(Voittaja) & Divari==input$divariRadio,Ottelu_ID])  
  } else {
    pelaamattomat <- unique(kaikkipelit[is.na(Voittaja),Ottelu_ID])
  }
  
  arpa<-ceiling(runif(1,0,length(pelaamattomat)))
  arvottu_ottelu_ID<-pelaamattomat[arpa]
  #eti ottelun pienin pelaamaton peli
  
  
  arvottu_peli_id <- kaikkipelit[Ottelu_ID==arvottu_ottelu_ID & is.na(Voittaja) , .SD[which.min(Ottelu_no)],.SDcols=c("peli_ID")][,peli_ID]
  paivitaSliderit(arvottu_peli_id,session)
  
  #print(pfi_data())
  print("arvo peli loppu")
  
})

output$data_vs_taulukko<-renderDataTable({
  req(input$radio_bo_mode,input$select_laurin_pakka,input$select_martin_pakka,input$numeric_MA_valinta,input$radio_pfi_mode)
  vs_statsit_MA<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,input$select_laurin_pakka,input$select_martin_pakka,input$numeric_MA_valinta,input$radio_pfi_mode,pfi_data())$transposed[(Tilasto %in% ("Voitot"))]
  
  vs_statsit_all<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,input$select_laurin_pakka,input$select_martin_pakka,NA,input$radio_pfi_mode,pfi_data())
  
  pakka_stats_all_lauri<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,input$select_laurin_pakka,NA,NA,input$radio_pfi_mode,pfi_data())$transposed[!(Tilasto %in% ("Voitot"))]
  laurin_pakkanimi<-colnames(pakka_stats_all_lauri)[3]
  pakka_stats_all_martti<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,NA,input$select_martin_pakka,NA,input$radio_pfi_mode,pfi_data())$transposed[!(Tilasto %in% ("Voitot"))]
  martin_pakkanimi<-colnames(pakka_stats_all_martti)[3]
  setkeyv(pakka_stats_all_lauri,c("Tilasto","selite"))
  setkeyv(pakka_stats_all_martti,c("Tilasto","selite"))   
  join_pakka_stats_all<-pakka_stats_all_lauri[pakka_stats_all_martti]
  
  
  #MA_pakak
  pakka_stats_MA_lauri<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,input$select_laurin_pakka,NA,input$numeric_MA_valinta,input$radio_pfi_mode,pfi_data())$transposed[(Tilasto %in% ("Voitot"))]
  pakka_stats_MA_martti<-sarjataulukkoKaikki(divaridata(),peliDataReact(),input$radio_bo_mode,1,TRUE,NA,NA,input$select_martin_pakka,input$numeric_MA_valinta,input$radio_pfi_mode,pfi_data())$transposed[(Tilasto %in% ("Voitot"))]
  
  
  pfistats<-sarjataulukkoKaikki(divaridata(),peliDataReact(),FALSE,1,TRUE,NA,NA,NA,NA,FALSE,pfi_data())$pfi_trans
  
  #ota vaan sarakkeet, mitä on muuallakkin käytetty
  pfi_subsetcols<-pfistats[,names(vs_statsit_all$transposed),with=FALSE]
  
  
  setkeyv(pakka_stats_MA_lauri,c("Tilasto","selite"))
  setkeyv(pakka_stats_MA_martti,c("Tilasto","selite"))   
  join_pakka_stats_MA<-pakka_stats_MA_lauri[pakka_stats_MA_martti]
  
  lisakortit<-funcLisakortit(peliDataReact(),divaridata(),turnausSaantoReact(),TRUE,pfi_data())$current_lisakortit
  
  #filtteröi mukaan vaan pelin pakat
  lisakortit_pelipakat<-lisakortit[(Omistaja=="Lauri" & Pakka==input$select_laurin_pakka)|(Omistaja=="Martti" & Pakka==input$select_martin_pakka),.(Nimi,Lisakortit,Tilasto="Pakan koko",selite="")]
  lisakortit_pelipakat[,':=' (Kortti_lkm=(floor(Lisakortit)+37),Lisakortit=NULL)]
  #transponoi
  
  lisakortit_trans<-data.table(dcast(lisakortit_pelipakat,Tilasto+selite~Nimi,value.var="Kortti_lkm"))
  lisakortit_final<-lisakortit_trans[,c(laurin_pakkanimi,"Tilasto","selite",martin_pakkanimi),with=FALSE]
  
  append<-rbind(vs_statsit_all$transposed,join_pakka_stats_all,vs_statsit_MA,join_pakka_stats_MA,pfi_subsetcols)#,laurin_MA$transposed)
  #vaihda sarakejärjestys
  result_table<-append[,c(laurin_pakkanimi,"Tilasto","selite",martin_pakkanimi),with=FALSE]
  #lisää vielä lisäkorttitilasto
  result_table<-rbind(result_table,lisakortit_final)
  
  return(result_table)  
  
},    options = list(
  paging = FALSE,
  
  searching = FALSE,
  info=FALSE,
  columnDefs = list(list(className = 'dt-center', targets = 1:2),
                    list(className = 'dt-left', targets = 3)),
  rowCallback = DT::JS(
    'function(row, data) {
    if ((data[2]) == "VS")
    $("td", row).css("background", "PaleTurquoise");
    else if (data[2] == "Deck" )
    $("td", row).css("background", "PapayaWhip");
    else if (data[2] == "pfi" )
    $("td", row).css("background", "PowderBlue");
    
    }')
    
  
  
  ),rownames=FALSE)