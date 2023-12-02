output$pfi_taulukko <- renderDataTable({
  #react on 
  refresh_counter$a
  ####
  required_data("STAT_PFI")
 
  return(STAT_PFI)
},    options = list(
  paging = FALSE,
  searching = FALSE,
  info = FALSE,
  rowCallback = DT::JS(
    'function(row, data) {
    // Bold cells for those >= 5 in the first column
    if (parseFloat(data[8]) >= 4)
    $("td", row).css("background", "Orange");
     if (parseFloat(data[7]) >= 4)
    $("td", row).css("background", "DodgerBlue");
      if (parseFloat(data[9]) == 0)
    $("td", row).css("background", "Red");}')
  
  ), rownames = FALSE)



