required_data(c("STAT_DMG_TURN_ALL", "ADM_TURN_SEQ"))
temp <- STAT_DMG_TURN_ALL
aggr_to_turn <- STAT_DMG_TURN_ALL[, .(Amount = sum(Amount)), by = .(Aloittaja, Turn,
                                                                    Target_player, Pakka_ID,
                                                                    Vastustajan_Pakka_ID, Omistaja_ID,
                                                                    Vastustajan_Omistaja_ID, Peli_ID)]
 
uniikit <- aggr_to_turn[, .N, by = .(Aloittaja,
                                     Target_player, Pakka_ID,
                                     Vastustajan_Pakka_ID, Omistaja_ID,
                                     Vastustajan_Omistaja_ID, Peli_ID)]
uniikit[, key := 1]
turn50 <- ADM_TURN_SEQ[Turn < 21, .N , by = (Turn)]
turn50[, key := 1]
mergeri <- merge(x = uniikit, y = turn50, by = "key", all = TRUE, allow.cartesian = TRUE)



joini <- aggr_to_turn[mergeri, on = .(Turn, Peli_ID, Aloittaja, Pakka_ID), allow.cartesian = TRUE]

joini[Peli_ID == 994]
