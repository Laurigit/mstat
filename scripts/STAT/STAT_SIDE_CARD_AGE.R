#find too old cards in side



required_data("STG_PFI")
required_data("SRC_CARDS_DIM")
required_data("SRC_CARDS")
required_data("SRC_DRAFT_CARDS")

sscols_cards_dim_all <- SRC_CARDS_DIM[, .(Type, Name)]

#fix - and  —
sscols_cards_dim_all[, Type := gsub("—", "-", Type)]

sscols_cards_dim <- sscols_cards_dim_all[, .N, by = .(Type, Name)][, N := NULL]

laurinmaxPID <- SRC_CARDS[Pakka_ID %in% c(21, 22), max(Pakka_form_ID), by = Pakka_ID][, V1]

sscols_cards <- SRC_CARDS[Pakka_form_ID %in% laurinmaxPID, .(Name, Pakka_form_ID, Maindeck, Pakka_ID, DRAFT_CARDS_ID )]

ss_draft <- SRC_DRAFT_CARDS[, .(DRAFT_CARDS_ID = id, PICK_ORDER, PICK_DT)]
join_dcid <- ss_draft[sscols_cards, on = "DRAFT_CARDS_ID"]
join_dcid[, key := 1]

sscolst_ajat <- STAT_TURNAUSAJAT[, .(Turnaus_NO, Aloitus_DT, Lopetus_DT,
                                     # turnaus_Aloitus_DT = Aloitus_DT,
                                     #turnaus_Lopetus_DT = Lopetus_DT,
                                     key = 1)]

#eka kartesian, sitten filtterillä ulos
inequijoini <- sscolst_ajat[join_dcid, on = "key", allow.cartesian = TRUE]

#filtterii
filtter_vaarat <- inequijoini[!(Aloitus_DT < PICK_DT & Lopetus_DT < PICK_DT) &
                                !(Aloitus_DT > PICK_DT & Lopetus_DT > PICK_DT) ]

required_data("ADM_PELIT")
max_turnaus <- ADM_PELIT[, max(Turnaus_NO)]

filtter_vaarat[, Card_Age := max_turnaus - Turnaus_NO]

STAT_SIDE_CARD_AGE <- filtter_vaarat[,. (Pakka_ID,
                                         Pakka_form_ID,
                                         
                                         Name,
                                         Omistaja_ID = ifelse(Pakka_ID == 21, "L", "M"),
                                         Side = 1,
                                         Maindeck,
                                         
                                         Turnaus_NO_drafted = Turnaus_NO,
                                         #Card_age = pmin(pmax(round((109 - Turnaus_NO * 1.25), 0), 10), 40) - (max_turnaus - Turnaus_NO)
                                         Card_age = Card_Age,
                                         PICK_ORDER)]
# Card_age = pmin(pmax(round((109 - Turnaus_NO * 1.25), 0), 10), 40) - (max_turnaus - Turnaus_NO))]

con <- connDB(con)
dbWT(con, STAT_SIDE_CARD_AGE)
# sidet <- STAT_SIDE_CARD_AGE[ Side == 1][, .N, by = .(Omistaja_ID, Card_age)][order(Card_age)]
# side_agg <- STAT_SIDE_CARD_AGE[Side == 1, sum(Count), by = .(Card_age, Omistaja_ID)][order(Card_age)]
# 
# vert <- dcast.data.table(side_agg, formula = Card_age ~ Omistaja_ID)
# vert[, .(l = sum(L), m = sum(M))]
#STAT_SIDE_CARD_AGE[Name == "Maze of Ith"]

