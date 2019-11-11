#save_all_missing_cards
#ADM_PAKKA_COMPONENTS
required_data("ADM_DI_HIERARKIA", TRUE)
required_data(c("STG_PAKKA_COMPONENTS", "STG_MANASTACK_CARDS", "STG_PFI", "STG_PAKAT"))
con <- connDB(con)
#find missing cards
joini <- STG_MANASTACK_CARDS[, .(NA_Filter = NA, Name)][STG_PAKKA_COMPONENTS[, .(Name)], on = "Name"]
all_names_in_CARDS_DIM <- dbQ("SELECT count(MID) as count_mid, Name from CARDS_DIM GROUP BY Name", con)[, Name]
all_names_in_DECKLISTS <- STG_PAKKA_COMPONENTS[, .N, by = Name][, Name]
missing_cards <- joini[is.na(NA_Filter), .N, by = Name][, Name]
missing_cards <- setdiff(all_names_in_DECKLISTS, all_names_in_CARDS_DIM)
new_cards <- NULL
counter <- 0

#missing_cards <- missing_cards[90:2500]
for(loop in missing_cards) {
  counter <- counter + 1
  print(paste0(counter, "/", length(missing_cards)))

  loop_cardMID <- getCard_from_SF(loop)
  new_cards <- rbind(new_cards, loop_cardMID)
 # loop_card[, Text := str_replace_all(Text, "[^[:alnum:]]", " ") ]
  
  dbIoU("CARDS_DIM", loop_cardMID, con)
 
}

#check if colname MID exists
sarakenimet <- colnames(STG_MANASTACK_CARDS)
onko_MID <- is.na(match("MID", sarakenimet))
if (onko_MID == TRUE) {
  STG_MANASTACK_CARDS[, MID := as.integer(NA)]
}
if (counter > 0) {
  STG_MANASTACK_CARDS <- rbind(STG_MANASTACK_CARDS, new_cards, fill = TRUE)
}

                        
STG_MANASTACK_CARDS[is.na(MID), MID := getCardMid(Name), by = Name]
STG_MANASTACK_CARDS[, ':=' (Name = iconv(x = Name, to = "UTF-8"),
                             Text = iconv(x = Text, to = "UTF-8"))]
MANASTACK_CARDS <- STG_MANASTACK_CARDS


saveR_and_send(MANASTACK_CARDS, "MANASTACK_CARDS", "MANASTACK_CARDS.RData")
updateData("SRC_MANASTACK_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())

join_additional_data <- MANASTACK_CARDS[STG_PAKKA_COMPONENTS, on = "Name"]
sscols_pfi <- STG_PFI[, .(Pakka_ID, Pakka_form_ID, is_current_form = Pakka_form_ID == Current_Pakka_form_ID)]
join_pakka_id <- sscols_pfi[join_additional_data, on = "Pakka_form_ID"]
sspakat <- STG_PAKAT[,. (Omistaja_ID, Pakka_NM, Pakka_ID, Side)]
joininmi <- sspakat[join_pakka_id, on = "Pakka_ID"]
ADM_PAKKA_COMPONENTS <- joininmi[Side == 0]
#MANASTACK_CARDS <- new_cards
#save(MANASTACK_CARDS,  file =  "./external_files/MANASTACK_CARDS.RData")
