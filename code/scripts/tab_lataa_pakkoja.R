output$pfi_taulukko <-renderDataTable({
  
  pfistats<-sarjataulukkoKaikki(divaridata(),peliDataReact(),FALSE,1,TRUE,NA,NA,NA,NA,FALSE,pfi_data())$pfi[!is.na(Nimi)][order(-Tappiot)]
  
  lisakortit<-funcLisakortit(peliDataReact(),divaridata(),turnausSaantoReact(),FALSE,pfi_data())$data
  
  #join
  
  joinLisakortit<-lisakortit[pfistats,on=c("Nimi")]
  return(joinLisakortit)
},    options = list(
  paging = FALSE,
  searching = FALSE,
  info=FALSE,
  rowCallback = DT::JS(
    'function(row, data) {
    // Bold cells for those >= 5 in the first column
    if (parseFloat(data[5]) >= 4)
    $("td", row).css("background", "Tomato");}')
  
  ),rownames=FALSE)
