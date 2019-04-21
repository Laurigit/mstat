#STAT_DMG_TURN_ALL 
#TÄÄ VOIS MENNÄ IHAN ADM_PUOLELLE AINAKI PERUSDATANA
required_data(c("ADM_DMG_TURN_ALL", "ADM_PELIT", "ADM_DIVARI", "ADM_TURN_SEQ"))

pakat <- ADM_PELIT[, .(Pakka_ID, Vastustajan_Pakka_ID, Peli_ID, Omistaja_ID, Vastustajan_Omistaja_ID, Aloittaja)]
temp <- ADM_DMG_TURN_ALL
pelatut_damagePelit<- ADM_DMG_TURN_ALL[, .N, by = Peli_ID][, N := NULL]

join_pakat <- pakat[pelatut_damagePelit, on = "Peli_ID"]
join_pakat[, count_of_matchups := .N , by = .(Pakka_ID, Vastustajan_Pakka_ID)]
join_pakat[, count_of_pakka_games := .N , by = .(Pakka_ID)]
join_pakat_ss <- join_pakat[, .(Pakka_ID, Vastustajan_Pakka_ID, count_of_matchups, count_of_pakka_games)]

div_temp <- ADM_DIVARI[,.(Pakka_ID, Divari, Omistaja_ID)]
l_pakat <- div_temp[Omistaja_ID == "L"][, Omistaja_ID := NULL]
m_pakat <- div_temp[Omistaja_ID == "M"][, Omistaja_ID := NULL]
setnames(m_pakat, "Pakka_ID", "Vastustajan_Pakka_ID")
joini <- l_pakat[m_pakat, on = "Divari"]
matsupit <- join_pakat_ss[joini, on = .(Pakka_ID, Vastustajan_Pakka_ID)]

#join_pakat[, used_Pakka_ID = ifelse(c)]

#general_setup
seq_data <- ADM_TURN_SEQ[, .(TSID, Turn, Starters_turn)]
join_seq <- seq_data[temp, on = "TSID"]
aloittajadata <- pakat[, .(Peli_ID, Aloittaja, Pakka_ID,
                                             Vastustajan_Pakka_ID, Omistaja_ID,
                           Vastustajan_Omistaja_ID)]
join_aloittaja <- aloittajadata[join_seq, on = "Peli_ID", allow.cartesian = TRUE]
#fix reverse damage ja combat damage- yhteys
join_aloittaja[, Combat_dmg := ifelse(Target_player == Dmg_source, 0, Combat_dmg)]
aggregate_overinput <- join_aloittaja[, .(Amount = mean(Amount)),
                                      by = .(Aloittaja, TSID, Turn, Starters_turn, Target_player,
                                             Dmg_source,
                                             Combat_dmg,
                                             Pakka_ID,
                                             Vastustajan_Pakka_ID,Omistaja_ID,
                                             Vastustajan_Omistaja_ID, Peli_ID)]
aggregate_overinput[, ':=' (Amount_dealt = ifelse(substr(Dmg_source, 1, 1) == Omistaja_ID & substr(Target_player, 1, 1) == Vastustajan_Omistaja_ID, Amount, 0),
                            Amount_received = ifelse(substr(Target_player, 1, 1) == Omistaja_ID, Amount, 0 ))]
setorder(aggregate_overinput, Peli_ID, TSID, Target_player)


# turn50 <- ADM_TURN_SEQ[Turn < 20,. (TSID)]
#
# joint20 <- merge(x = aggregate_overinput, y = turn50, by = "TSID", all.y = TRUE)
# joint20 <- CJ_dt(turn50, aggregate_overinput)
# joint20[Peli_ID == 994 & TSID == 1]

STAT_DMG_TURN_ALL <- aggregate_overinput
# 
# #join_seq[, half_turn := (Turn  + ifelse(Starters_turn == TRUE, 0, 0.5))]
# #aver_dmg_dealt <- join_seq[, .(ave_dmg = mean(Amount)), by = .(Target_player, Turn)]
# #case not enough games or matchups
# #case_tot <- temp[,]


#line: How much damage my deck deals on average per turn? my + opponent self damage
#line: How much damage my deck takes on average per turn? Oppoenent + own self damage
#lisää joskus alottaja
