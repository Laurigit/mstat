#ADM_TURN_DATA_ALL
required_data(c("STG_TURN_DATA_ALL", "ADM_TURN_SEQ", "ADM_PELIT"))
temp  <- STG_TURN_DATA_ALL
aggr_tuplarivit <- temp[,. (max_posix_aika = max(posix_aika)), by = .(TSID, Peli_ID)]



#join SEQ
ss_seq <- ADM_TURN_SEQ[,. (TSID, Turn, End_phase, Starters_turn)]
joinseq <- ss_seq[aggr_tuplarivit, on = "TSID"]


pakat <- ADM_PELIT[, .(Pakka_ID, Vastustajan_Pakka_ID, Peli_ID, Omistaja_ID, Vastustajan_Omistaja_ID, Aloittaja)]
aloittajadata <- pakat[Omistaja_ID == "L", .(Peli_ID, Lauri_aloitti = Aloittaja, Laurin_pakka = Pakka_ID, Martin_pakka = Vastustajan_Pakka_ID)]
join_aloittaja <- aloittajadata[joinseq, on = "Peli_ID"]
join_aloittaja[, ':=' (Laurin_vuoro = ifelse(Lauri_aloitti ==  Starters_turn, 1, 0))]
#sortta
setorder(join_aloittaja, Peli_ID, TSID)
join_aloittaja[, vuoroKesto := max_posix_aika - lag(max_posix_aika), by = Peli_ID]
filtterNA <- join_aloittaja[!is.na(vuoroKesto)]
filtterNA[, vuoroKesto_min := as.numeric(vuoroKesto / 60)]
ADM_TURN_DATA_ALL <- filtterNA
