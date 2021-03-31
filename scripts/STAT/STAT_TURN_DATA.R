"STAT_TURN_DATA"
required_data(c("ADM_TURN_DATA_ALL", "STG_DIVARI"))
temp <- ADM_TURN_DATA_ALL
aggrew_L <- temp[Laurin_vuoro == 1 & vuoroKesto > 0 & vuoroKesto < 180, .(median_kesto = median(vuoroKesto)), by = .(Pakka = Laurin_pakka, End_phase)]
aggrew_L[, Omistaja_ID := "L"]
aggrew_L[, Turn := -1]
aggrew_M <- temp[Laurin_vuoro == 0 & vuoroKesto > 0 & vuoroKesto < 180, .(median_kesto = median(vuoroKesto)), by = .(Pakka = Martin_pakka, End_phase)]
aggrew_M[, Turn := -1]
aggrew_M[, Omistaja_ID := "M"]


aggrew_L2 <- temp[Laurin_vuoro == 1 & vuoroKesto > 0 & vuoroKesto < 180, .(median_kesto = median(vuoroKesto)), by = .(Pakka = Laurin_pakka, End_phase, Turn)]
aggrew_L2[, Omistaja_ID := "L"]

aggrew_M2 <- temp[Laurin_vuoro == 0 & vuoroKesto > 0 & vuoroKesto < 180, .(median_kesto = median(vuoroKesto)), by = .(Pakka = Martin_pakka, End_phase, Turn)]

aggrew_M2[, Omistaja_ID := "M"]








appendaa <- rbind(aggrew_L, aggrew_M, aggrew_L2, aggrew_M2)
appendaa[, mean(median_kesto), by = .(Omistaja_ID, End_phase)]
ss_divarti <- STG_DIVARI[, .(Pakka_ID, Deck_name)]
join_name <- ss_divarti[appendaa, on = .(Pakka_ID == Pakka)]
join_name[, median_kesto_numeric := as.numeric(median_kesto)]
setnames(join_name, "Deck_name", "Pakka_NM")
STAT_TURN_DATA <- join_name
