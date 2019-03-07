#STAT_DMG_TURN_ALL 
required_data(c("ADM_DMG_TURN_ALL", "ADM_PELIT", "ADM_DIVARI", "ADM_TURN_SEQ"))

pakat <- ADM_PELIT[, .(Pakka_ID, Vastustajan_Pakka_ID, Peli_ID, Omistaja_ID, Vastustajan_Omistaja_ID)]
temp <- ADM_DMG_TURN_ALL
pelatut_damagePelit<- ADM_DMG_TURN_ALL[, .N, by = Peli_ID][, N := NULL]

join_pakat <- pakat[pelatut_damagePelit, on = "Peli_ID"]
join_pakat[, count_of_matchups := .N , by = .(Pakka_ID, Vastustajan_Pakka_ID)]
join_pakat[, count_of_pakka_games := .N , by = .(Pakka_ID)]


#general_setup 
seq_data <- ADM_TURN_SEQ[, .(TSID, Turn, Starters_turn)]
join_seq <- seq_data[temp, on = "TSID"]
join_seq[, half_turn := (Turn  + ifelse(Starters_turn == TRUE, 0, 0.5))]
aver_dmg_dealt <- join_seq[, .(ave_dmg = mean(Amount)), by = .(half_turn, Target_player)]
#case not enough games or matchups
#case_tot <- temp[,]
