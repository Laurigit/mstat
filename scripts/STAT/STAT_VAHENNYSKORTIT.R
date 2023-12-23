#ADM_VAHENNYSKORTIT
required_data("STG_VAHENNYSKORTIT")
required_data("STG_DRAFT_CARDS")
required_data("STAT_TURNAUSAJAT")
required_data("STG_CARDS")
required_data("STG_PAKAT")
eisidet <- STG_PAKAT[Side == 0, Pakka_ID]

#STG_VAHENNYSKORTIT <- rbind(STG_VAHENNYSKORTIT, data.table(Turnaus_NO = 80, vahennyskortit_ykkospick  = 4, jakaja = 2))


max_id <- STG_CARDS[, .(Pakka_form_ID = max(Pakka_form_ID)), by = Pakka_ID]
filter_max <- STG_CARDS[Pakka_form_ID %in% max_id[, Pakka_form_ID]]

ss_cards <- filter_max[DRAFT_CARDS_ID > 0,. (DRAFT_CARDS_ID, Valid_from_Date, Pakka_ID)]
aggr_cards <- ss_cards[Pakka_ID %in% eisidet,. (Valid_from_Date = min(Valid_from_Date)), by = .(DRAFT_CARDS_ID, Pakka_ID)]

ss_draft <-  STG_DRAFT_CARDS[PICKED == 1, .(PICK_ORDER, DRAFT_CARDS_ID, PICK_DT)]#[DRAFT_CARDS_ID == 5]

join_draft_and_decklists <- ss_draft[aggr_cards, on = "DRAFT_CARDS_ID"][!is.na(PICK_ORDER)]

ss_jat <- STAT_TURNAUSAJAT[, .(alku = as.IDate(Aloitus_DT), lopp = as.IDate(Lopetus_DT), Turnaus_NO)]

joini <- join_draft_and_decklists[ss_jat, on = .(PICK_DT > alku, PICK_DT <= lopp)][!is.na(PICK_ORDER)]#[PICK_DT >= Valid_from_Date & PICK_DT <= Valid_from_Date.1 ]
joini[Turnaus_NO == 89]

#tavoitedata. #PICK_ORDER, Turnaus_NO, Pakka_ID
turnaussaanto<- copy(STG_VAHENNYSKORTIT)
#levita saannot
turnauksia<-data.table(TurnausNoSeq=1:1000)
#setwd("C:/Users/Lauri/Documents/R/mstat2/code/omawd")
levite<-data.table(expand.grid(1:10000))
setnames(levite,c("Var1"),c("Turnaus_NO"))
joinsaanto <- turnaussaanto[levite,on=c("Turnaus_NO")]
joinsaanto<-joinsaanto[order(Turnaus_NO)]
#korvaa NA:t seuraavalla

joinsaanto[, vahennyskortit_ykkospick  := na.locf(vahennyskortit_ykkospick , na.rm = FALSE)]
joinsaanto[, jakaja  := na.locf(jakaja , na.rm = FALSE)]

join_vahennys <- joini[joinsaanto, on = "Turnaus_NO"][!is.na(PICK_ORDER)]
join_vahennys[, calc_jakaja := jakaja ^ (ceiling(PICK_ORDER / 2) - 1)]
join_vahennys[, calc_vahennyskortit := vahennyskortit_ykkospick / calc_jakaja]
join_vahennys[, Pakka_ID := as.character(Pakka_ID)]
STAT_VAHENNYSKORTIT <- join_vahennys[, .(vahennyskortit = sum(calc_vahennyskortit)), by = Pakka_ID]

