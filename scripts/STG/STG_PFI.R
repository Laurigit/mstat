#STG_PFI
required_data(c("SRC_CARDS", "SRC_DIVARI"))
#datetime, Pakka_ID, kortti_lkm, hinnat,

#muuttuja Sum_maindeck_DCID yrittää tarkailla, onko maindeckiin tehty muutoksia

pakkametataulu_uus <- SRC_CARDS[, .(Sum_maindeck_DCID = sum((as.integer(Maindeck) * as.integer(DRAFT_CARDS_ID)))/100000,
                                                            Kortti_lkm_manastack = sum(as.integer(Maindeck)),
                                    Valid_from_DT = max(Valid_from_DT)), by = .(Pakka_form_ID, Pakka_ID)]


pakkametataulu_uus[, ':=' (Hinta = 1,
                           Hinta_low = 1,
                           Hinta_high = 1,
                           Valid_from_DT = as.POSIXct(Valid_from_DT, tz ="EET"))]



pakka_meta_sorted <- pakkametataulu_uus[order(Pakka_ID, Valid_from_DT)]
pakka_meta_sorted[,':=' (Valid_to_DT = shift(Valid_from_DT,1,type="lead")),
                  by = .(
                    Pakka_ID)]
pakka_meta_sorted[,':=' (Valid_to_DT = as.POSIXct((ifelse(is.na(Valid_to_DT),
                                                          as.POSIXct("2100-01-01", tz = "EET"),
                                                          Valid_to_DT)), tz = "EET", origin = "1970-01-01"))]
#etsi pakan viimeisin Sum_maindeck_DCID
max_pfi <- pakka_meta_sorted[, .(max_pfi = max(Pakka_form_ID)), by = .(Pakka_ID)]
last_Sum_maindeck_DCID <- pakka_meta_sorted[Pakka_form_ID %in% max_pfi[, max_pfi], .(LAST_Sum_maindeck_DCID = Sum_maindeck_DCID, Pakka_ID)]
#joinaa tulos
join_LAST <- last_Sum_maindeck_DCID[pakka_meta_sorted, on = .(Pakka_ID)]
#etsi pienin pfi eli vanhin versio, jossa maindeck on vakio.
curr_PFI <- join_LAST[LAST_Sum_maindeck_DCID == Sum_maindeck_DCID, .(Current_Pakka_form_ID = min(Pakka_form_ID)), by = Pakka_ID]
#joinaa current_PFI
join_curr_PFI <- curr_PFI[join_LAST, on = .(Pakka_ID)]


#pakka_meta_sorted[, Current_Pakka_form_ID := max(Pakka_form_ID), by = Pakka_ID]
join_curr_PFI[, Pakka_ID := as.character(Pakka_ID)]
join_curr_PFI[, LAST_Sum_maindeck_DCID := NULL]
join_curr_PFI[, Sum_maindeck_DCID := NULL]
STG_PFI <- join_curr_PFI
