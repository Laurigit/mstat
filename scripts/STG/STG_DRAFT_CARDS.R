#STG_DRAFT_CARDS
required_data("SRC_DRAFT_CARDS")

STG_DRAFT_CARDS <- SRC_DRAFT_CARDS[, .(DRAFT_CARDS_ID = id, DRAFT_ID, MID, PICK_ORDER, OMISTAJA_ID, PICK_DT =  as.IDate(PICK_DT),
                                       PICKED)]
