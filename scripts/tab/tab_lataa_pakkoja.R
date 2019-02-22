output$pfi_taulukko <- renderDataTable({
  #react on 
  refresh_counter$a
  ####
  required_data("STAT_PFI")
  # required_data(c("STAT_LISAKORTIT", "ADM_PELIT", "STG_PAKAT"))
  # maxturnaus_per_pakka <- STAT_LISAKORTIT[, .(Turnaus_NO = max(Turnaus_NO)), by = .(Pakka_ID)]
  # onlymax <- STAT_LISAKORTIT[maxturnaus_per_pakka, on = c("Turnaus_NO", "Pakka_ID")]
  # required_data("STG_PFI")
  # current_pfi_by_pakka <- STG_PFI[Pakka_form_ID == Current_Pakka_form_ID, .(Pakka_form_ID, Pakka_ID)]
  # bo_conv_pelit <- BO_conversio(ADM_PELIT)
  # sscols_pelit <- bo_conv_pelit[,. (Pakka_ID, Voittaja, Tasapeli, Peli_LKM, Pakka_form_ID)]
  # nykypakan_tilastot <- sscols_pelit[current_pfi_by_pakka, on = .(Pakka_form_ID, Pakka_ID)]
  # aggr_to_pakka <- nykypakan_tilastot[ , .(Voitot = sum(Voittaja * Peli_LKM, na.rm = TRUE),
  #                                         Tasapelit = sum(Tasapeli * Peli_LKM, na.rm = TRUE),
  #                                         Peli_LKM = sum(Peli_LKM * !is.na(Voittaja)) ),
  #                                     by = .(Pakka_ID, Pakka_form_ID)]
  # aggr_to_pakka[, Pfi_voitot := (Voitot + Tasapelit * 0.5), by = Pakka_ID]
  # aggr_to_pakka[, Pfi_tappiot := Peli_LKM - Pfi_voitot]
  # sspakat <- STG_PAKAT[, .(Pakka_ID, Omistaja_ID, Pakka_NM)]
  # joinpakat <- sspakat[aggr_to_pakka, on = "Pakka_ID"]
  # #joinlisakortit
  # joinlisa <- onlymax[joinpakat, on = "Pakka_ID"]
  # res_table <- joinlisa[, .(Deck = Pakka_NM, Owner = Omistaja_ID, Deck_size = Lisakortit_lkm, Wins = Pfi_voitot, Lossees = Pfi_tappiot)]
  # setorder(res_table, -Wins)
  
  return(STAT_PFI)
},    options = list(
  paging = FALSE,
  searching = FALSE,
  info = FALSE,
  rowCallback = DT::JS(
    'function(row, data) {
    // Bold cells for those >= 5 in the first column
    if (parseFloat(data[5]) >= 4)
    $("td", row).css("background", "Orange");
     if (parseFloat(data[4]) >= 4)
    $("td", row).css("background", "DodgerBlue");
      if (parseFloat(data[6]) == 0)
    $("td", row).css("background", "Red");}')
  
  ), rownames = FALSE)


observeEvent(input$input_lataa_valitut_pakat,{
  required_data(c("STAT_PFI", "STG_DIVARI"))
 # updateData(STG_DIVARI, ADM_DI_HIERARKIA, input_env = globalenv())
  #input <- NULL
  #input$pfi_taulukko_rows_selected <- c(1,5)
  shinyjs::disable("input_lataa_valitut_pakat")
  valitut_rivit <- input$pfi_taulukko_rows_selected
  ladattavat_pakat <- STAT_PFI[valitut_rivit, .(Deck)]

  validointi_teksti <- getDeckByID(ladattavat_pakat, STG_DIVARI)
  zip_all_and_send()
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_PFI", ADM_DI_HIERARKIA, input_env = globalenv(), rewriteSaveR = FALSE)
  
  refresh_counter$a <- isolate(refresh_counter$a +1 )
  validointiteksti$teksti <- validointi_teksti
  shinyjs::enable("input_lataa_valitut_pakat")
})
