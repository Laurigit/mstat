output$pfi_taulukko <- renderDataTable({
  
  pfistats <- sarjataulukkoKaikki(divaridata(),peliDataReact(),FALSE,1,TRUE,NA,NA,NA,NA,FALSE,pfi_data())$pfi[!is.na(Nimi)][order(-Tappiot)]
print("pfistats =")
print(pfistats)
  lisakortit <- funcLisakortit(peliDataReact(),divaridata(),turnausSaantoReact(),FALSE,pfi_data())$data
  print("lisakorit =")
  print(lisakortit)
  #join
  print("LISÄKORIT alko")
  joinLisakortit <- lisakortit[pfistats, on = c("Nimi")]
  print(joinLisakortit)
  
 
  rjs <-react_omaReadJson()$viimenen_pfi
  ss_cols_read_json <- rjs[, .(id, Pakka, Omistaja)]
  print("rjs=")
  print(rjs)

  #join rjs
  joined_rjs <- ss_cols_read_json[joinLisakortit, on = c("Pakka", "Omistaja")]
  print("joined_rjs =")
  print(joined_rjs)
  #jos pakan id > pfi, nin silloin siitä on upattu uusi versio ja voitto ja tappiot = 0
  joined_rjs[, ':=' (Voitot  = ifelse(id > pfi, 0, Voitot),
                     Tappiot = ifelse(id > pfi, 0, Tappiot),
                     id = NULL,
                     pfi = NULL)]
  joined_rjs<-joined_rjs[order(-Tappiot)]
  print(joined_rjs)
  
  print("LISÄKORIT")
  print(joinLisakortit)
  return(joined_rjs)
},    options = list(
  paging = FALSE,
  searching = FALSE,
  info = FALSE,
  rowCallback = DT::JS(
    'function(row, data) {
    // Bold cells for those >= 5 in the first column
    if (parseFloat(data[5]) >= 4)
    $("td", row).css("background", "Tomato");
     if (parseFloat(data[4]) >= 4)
    $("td", row).css("background", "DodgerBlue");}')
  
  ), rownames = FALSE)
