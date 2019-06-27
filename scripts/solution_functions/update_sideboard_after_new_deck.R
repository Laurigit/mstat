#tehtävä: vähennä decklististä toisen decklistin kortit
#input poistettavat, decklist_orig
#output decklist_orig_reduced, poistettavat_rest
# input_new_deck_id <- 17
# omistaja_input <- "L"

update_sideboard_after_new_deck <- function(omistaja_input, input_new_deck_id) {
  required_data("STG_PAKAT")
  required_data("STG_PAKKA_COMPONENTS")
  required_data("STG_PFI")

  sidet <- STG_PAKAT[Omistaja_ID == omistaja_input & Side == 1,. (Pakka_ID, Pakka_NM)]
  curr_comp <- STG_PAKKA_COMPONENTS[STG_PFI[Current_Pakka_form_ID == Pakka_form_ID], on = "Pakka_form_ID"]
  new_deck_list <- curr_comp[Pakka_ID == input_new_deck_id, .( Count, Name)][order(Name)]

  for (sideloop_no in 1:nrow(sidet)) {
  sideloop <- sidet[sideloop_no, Pakka_ID]
  sideName <- sidet[sideloop_no, Pakka_NM]
  
  sideloop_id_list <- curr_comp[Pakka_ID == sideloop, .( Count, Name)]
  
  newLists <-   remove_used_cards(sideloop_id_list, new_deck_list)
  jaljella <- new_deck_list[, sum(Count)]
  side_updated_temp <- newLists$decklist_reduced[order(Name)]
  side_updated <- side_updated_temp[, .(Count, Name)]
  print(side_updated)
  poistettu <- sideloop_id_list[, sum(Count)] - side_updated[, sum(Count)]
  
  new_deck_list <- newLists$removed_list_left
  lisatty <- jaljella - new_deck_list[, sum(Count)] 
  print(poistettu)
  print(lisatty)
  write.table(x = side_updated,
              file = paste0("./", paste0(sideName,".txt")),
              sep = " ",
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE,
              dec = ",")
  }
}
#update_sideboard_after_new_deck("L", 17)
