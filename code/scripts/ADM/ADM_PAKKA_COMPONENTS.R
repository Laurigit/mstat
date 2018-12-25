#save_all_missing_cards
#ADM_PAKKA_COMPONENTS
required_data("ADM_DI_HIERARKIA", TRUE)
required_data(c("STG_PAKKA_COMPONENTS", "STG_MANASTACK_CARDS", "STG_PFI", "STG_PAKAT"))
#find missing cards
joini <- STG_MANASTACK_CARDS[, .(Card_ID, Name)][STG_PAKKA_COMPONENTS[, .(Card_ID)], on = "Card_ID"]
missing_cards <- joini[is.na(Name), .N, by = Card_ID][, Card_ID]
new_cards <- NULL
counter <- 0
for(loop in missing_cards) {
  counter <- counter + 1
  print(paste0(counter, "/", length(missing_cards)))
  loop_card <- getCard(loop)
  new_cards <- rbind(new_cards, loop_card)
  print(loop_card)
}
MANASTACK_CARDS <- rbind(MANASTACK_CARDS, new_cards)
saveR_and_send(MANASTACK_CARDS, "MANASTACK_CARDS", "MANASTACK_CARDS.RData")
updateData("SRC_MANASTACK_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())

join_additional_data <- MANASTACK_CARDS[STG_PAKKA_COMPONENTS, on = "Card_ID"][, i.Name := NULL]
sscols_pfi <- STG_PFI[, .(Pakka_ID, Pakka_form_ID, is_current_form = Pakka_form_ID == Current_Pakka_form_ID)]
join_pakka_id <- sscols_pfi[join_additional_data, on = "Pakka_form_ID"]
sspakat <- STG_PAKAT[,. (Omistaja_ID, Pakka_NM, Pakka_ID)]
joininmi <- sspakat[join_pakka_id, on = "Pakka_ID"]
ADM_PAKKA_COMPONENTS <- joininmi
#MANASTACK_CARDS <- new_cards
#save(MANASTACK_CARDS,  file =  "./external_files/MANASTACK_CARDS.RData")
