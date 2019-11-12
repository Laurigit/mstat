#STG_PAKKA_COMPONENTS
required_data("SRC_PFI")
alldecks <- NULL
count_decks <- length(SRC_PFI)
for (pakka_no in 1:count_decks){ 
  
  pfi <- SRC_PFI[[pakka_no]]$pakka_form_id
  SRC_PFI[[pakka_no]]$grouped$cards
  counter <- 0
  deck_total <- NULL
  for(card_type in SRC_PFI[[pakka_no]]$grouped$name){
    counter <- counter + 1

    if(nrow(SRC_PFI[[pakka_no]]$grouped$cards[[counter]]) > 0) {
    row <- data.table(SRC_PFI[[pakka_no]]$grouped$cards[[counter]])[,. (pfi, count, name, type, card_type, id)]
    deck_total <- rbind(deck_total, row)
    }
   
  }
  alldecks <- rbind(alldecks, deck_total)
 
  
}
alldecks[, ':=' (Maindeck = ifelse(card_type == "Sideboard", FALSE, TRUE),
                 Type_exact = word(type, start = 1, sep = " — "),
                 Tribe_total = word(type, start = 2, sep =" — ")
                 )]
alldecks[, ':=' (Race = word(Tribe_total, start = 1, sep = " "),
                 Class = word(Tribe_total, start = 2, sep = " "),
                 Subclass = word(Tribe_total, start = 3, sep = " "),
                 Subtype = word(Type_exact, start = -2, end = -2, sep = " "))]



#get all different card types
# alldecks[, .N, by = Card_type_exact][, .N, by = Card_type_exact][, Card_type_exact]
#                    
#                    word(type, -2, sep = fixed(" ")) )]
 
STG_PAKKA_COMPONENTS <- alldecks[, .(Pakka_form_ID = pfi,
                                     Card_ID = id,
                                     Count = count,
                                     Name = gsub("Æ", "Ae", name),
                                     Maindeck,
                                     Type_and_Class = "",
                                     Type = card_type,
                                     Subtype,
                                     Type_exact,
                                     Tribe_total,
                                     Race,
                                     Class,
                                     Subclass
                                     )]
