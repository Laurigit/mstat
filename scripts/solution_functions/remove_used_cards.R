# #tehtävä: vähennä decklististä toisen decklistin kortit
# #input poistettavat, decklist_orig
# #output decklist_orig_reduced, poistettavat_rest
# 
# required_data("STG_PFI")
# required_data("STG_PAKAT")
# required_data("STG_MANASTACK_CARDS")
# required_data("STG_PAKKA_COMPONENTS")
# required_data("STAT_CURRENT_PAKKA_COMPONENTS")
# 
# #luo esimerkki decklist_orig
# curr_comp <- STG_PAKKA_COMPONENTS[STG_PFI[Current_Pakka_form_ID == Pakka_form_ID], on = "Pakka_form_ID"]
# removed_cards_input <- curr_comp[Pakka_ID == 6, .(Count, Name)]
# decklist_orig_input <- curr_comp[Pakka_ID == 31, .(Count, Name)]


remove_used_cards <- function(decklist_orig_input, removed_cards_input) {
  #pitää aggregoida, koska voi olla sama nimi eri riveillä, koska niillä eri id(eli setti tai kuva)
  
removed_cards <- removed_cards_input[,. (Count_R = sum(Count)), by = Name]
decklist_orig <- decklist_orig_input[, .(Count = sum(Count)), by = Name]

 result <- NULL
  joinaa <- removed_cards[decklist_orig, on = c("Name")]
  joinaa[, Count_R_fix_NA := ifelse(is.na(Count_R), 0, Count_R )]
  #laske vähennyksen jälkeen jäljelle jäävä lkm
  joinaa[, Count_after := Count - Count_R_fix_NA]
  
  result$decklist_reduced <- joinaa[Count_after > 0, .(Name, Count = Count_after)]
  
  #toiseen suuntaan removed_cards
  joinaa2 <- decklist_orig[removed_cards, on = c("Name")]
  joinaa2[, Count_fix_NA := ifelse(is.na(Count), 0, Count )]
  #laske vähennyksen jälkeen jäljelle jäävä lkm
  joinaa2[, Count_after := Count_R - Count_fix_NA]
  result$removed_list_left <- joinaa2[Count_after > 0, .(Name, Count = Count_after)]
  return(result)
}
