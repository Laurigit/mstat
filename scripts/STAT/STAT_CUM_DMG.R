#STAT_CUM_DMG
required_data(c("STAT_DMG_TURN_ALL", "ADM_TURN_SEQ", "STG_PAKAT", "ADM_PELIT"))
temp <- STAT_DMG_TURN_ALL
aggr_to_turn <- STAT_DMG_TURN_ALL[, .(Amount = sum(Amount)), by = .(Aloittaja, Turn,
                                                                    Target_player, Pakka_ID,
                                                                    Vastustajan_Pakka_ID, Omistaja_ID,
                                                                    Vastustajan_Omistaja_ID, Peli_ID)]

 
uniikit <- aggr_to_turn[, .(last_turn = max(Turn)), by = .(Aloittaja,
                                      Pakka_ID,
                                     Vastustajan_Pakka_ID, Omistaja_ID,
                                     Vastustajan_Omistaja_ID, Peli_ID)]
uniikit[, key := 1]

turn50 <- ADM_TURN_SEQ[Turn < 21, .N , by = (Turn)]
turn50[, key := 1]
mergeri <- merge(x = uniikit, y = turn50, by = "key", all = TRUE, allow.cartesian = TRUE)


aggr_to_turn[Peli_ID == 994]
joini <- aggr_to_turn[mergeri, on = .(Turn, Peli_ID, Aloittaja, Pakka_ID, Vastustajan_Pakka_ID,
                                      Vastustajan_Omistaja_ID,
                                      Omistaja_ID), allow.cartesian = TRUE]#[Peli_ID > 1320]
joini[, Amount := ifelse(is.na(Amount), 0, Amount)]
joini[Peli_ID == 994][order(Peli_ID, Pakka_ID, Turn)]
joini[str_sub(Target_player, 1 , 1) != Omistaja_ID, cum_dmg_dealt_temp := cumsum(Amount), by = .(Target_player, Pakka_ID, Peli_ID)]
joini[str_sub(Target_player, 1 , 1) == Omistaja_ID, cum_dmg_received_temp := cumsum(Amount), by = .(Target_player, Pakka_ID, Peli_ID)]
joini[, Dealt_damage_on_turn := ifelse(Amount > 0 & str_sub(Target_player, 1 , 1) != Omistaja_ID, 1, 0)]
joini[, Received_damage_on_turn := ifelse(Amount > 0 & str_sub(Target_player, 1 , 1) == Omistaja_ID, 1, 0)]
joini[, Dealt_damage_on_turn := ifelse(Turn > last_turn, NA, Dealt_damage_on_turn)]
joini[, Received_damage_on_turn := ifelse(Turn > last_turn, NA, Received_damage_on_turn)]
joini[, Amount := ifelse(Turn > last_turn, NA, Amount)]
joini[, ':=' (key = NULL, N = NULL)]
joini[ , cum_dmg_dealt := na.locf(cum_dmg_dealt_temp, na.rm = FALSE), by = .(Pakka_ID, Peli_ID)]
joini[ , cum_dmg_received := na.locf(cum_dmg_received_temp, na.rm = FALSE), by = .(Pakka_ID, Peli_ID)]
joini[, ':=' (cum_dmg_dealt = ifelse(is.na(cum_dmg_dealt), 0, cum_dmg_dealt),
              cum_dmg_received = ifelse(is.na(cum_dmg_received), 0, cum_dmg_received),
              cum_dmg_dealt_temp = NULL,
              cum_dmg_received_temp = NULL)]
filter <- joini#[Turn <= last_turn]

nimet <-STG_PAKAT[, .(Pakka_ID, Pakka_NM)]
nimet2 <-STG_PAKAT[, .(Vastustajan_Pakka_ID = Pakka_ID, Vastustajan_Pakka_NM = Pakka_NM)]
joini_nimi <- nimet[filter, on = "Pakka_ID"]
joini_nimi2 <- nimet2[joini_nimi, on = ("Vastustajan_Pakka_ID")]

voittaja <- ADM_PELIT[, .(Pakka_ID, Vastustajan_Pakka_ID, Voittaja, Peli_ID)]
join_voittaja <- voittaja[joini_nimi2, on = .(Peli_ID, Pakka_ID, Vastustajan_Pakka_ID)]
join_voittaja[cum_dmg_dealt > 30]
join_voittaja[Peli_ID == 1040]
join_voittaja[, lethal_damage := ifelse(Turn == last_turn & cum_dmg_dealt >= 20, 1, 0)]
join_voittaja[, lethal_damage_capped_to_20 := ifelse(Turn == last_turn & cum_dmg_dealt >= 20, Amount + 20 - cum_dmg_dealt, NA)]
join_voittaja[, extra_damage_over_20 := ifelse(Turn == last_turn & cum_dmg_dealt >= 20, cum_dmg_dealt - 20, NA)]
join_voittaja[, lifegain_amount := ifelse(Amount < 0, -Amount, 0)]
STAT_CUM_DMG <- join_voittaja


