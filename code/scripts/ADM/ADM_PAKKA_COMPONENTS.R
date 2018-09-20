#save_all_missing_cards
#ADM_PAKKA_COMPONENTS
required_data("ADM_DI_HIERARKIA", TRUE)
required_data(c("STG_PAKKA_COMPONENTS", "STG_MANASTACK_CARDS"), force_update = TRUE)
#find missing cards
joini <- STG_MANASTACK_CARDS[, .(Card_ID, Name)][STG_PAKKA_COMPONENTS[, .(Card_ID)], on = "Card_ID"]
missing_cards <- joini[is.na(Name), .N, by = Card_ID][, Card_ID]
new_cards <- NULL
for(loop in missing_cards) {
  loop_card <- getCard(loop)
  new_cards <- rbind(new_cards, loop_card)
  #print(loop_card)
}
MANASTACK_CARDS <- rbind(MANASTACK_CARDS, new_cards)
saveR_and_send(MANASTACK_CARDS, "MANASTACK_CARDS", "MANASTACK_CARDS.RData")

join_additional_data <- MANASTACK_CARDS[STG_PAKKA_COMPONENTS, on = "Card_ID"][, i.Name := NULL]
ADM_PAKKA_COMPONENTS <- join_additional_data
#MANASTACK_CARDS <- new_cards
#save(MANASTACK_CARDS,  file =  "./external_files/MANASTACK_CARDS.RData")
