#STG_CARDS
required_data("SRC_CARDS")
STG_CARDS <- SRC_CARDS[,. (MID, Pakka_ID, DRAFT_CARDS_ID,
                           Pakka_form_ID,
                    
                           Name,
                           Maindeck,
                           Valid_from_DT =  as.POSIXct(Valid_from_DT, format = "%Y-%m-%d %H:%M:%S"),
                           Valid_from_Date = as.IDate(Valid_from_DT))]
