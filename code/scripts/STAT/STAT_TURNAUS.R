#STAT_TURNAUS
required_data("ADM_PELIT")
required_functions("NO_conversio")
BO_data <- BO_conversio(ADM_PELIT)
# aggr_to_turnaus <- BO_data[!is.na(Aloitus_DT),.(Pakka_ID_list = list(unique(Pakka_ID)),
#                                 #Vasustajan_ID_list = list(Vastustajan_Pakka_ID),
#                                 Aloitus_DT = min(Aloitus_DT),
#                                 Lopetus_DT = max(Lopetus_DT),
#                                 Voittaja_sum = sum(Voittaja * Peli_LKM),
#                                 Peli_LKM_sum = sum(Peli_LKM, na.rm = TRUE),
#                                 Tasapeli_sum = sum(Tasapeli * Peli_LKM)
#                                 ),
#                            by = .(Turnaus_NO, Omistaja_ID)]

aggr_to_turnaus <- BO_data[,.(Pakka_ID_list = list(unique(Pakka_ID)),
                              Divari_list = list(unique(Divari)),
                                                #Vasustajan_ID_list = list(Vastustajan_Pakka_ID),
                                                Aloitus_DT = min(Aloitus_DT, na.rm = TRUE),
                                                Lopetus_DT = max(Lopetus_DT, na.rm = TRUE),
                                                Voittaja_sum = sum(Voittaja * Peli_LKM, na.rm = TRUE),
                                                Peli_LKM_sum = sum(Peli_LKM, na.rm = TRUE),
                                                Tasapeli_sum = sum(Tasapeli * Peli_LKM, na.rm = TRUE)
),
by = .(Turnaus_NO, Omistaja_ID)]

aggr_to_turnaus[, Pelatut_pelit := sum(Voittaja_sum + Tasapeli_sum / 2), by = .(Turnaus_NO)]
aggr_to_turnaus[, Turnaus_valmis := Peli_LKM_sum == Pelatut_pelit]

aggr_to_Pakka_turnaus <- ADM_PELIT[, .(sum_voitot = sum(Voittaja)), by = .(Pakka_ID, Turnaus_NO, Divari)]
aggr_to_Pakka_turnaus[, ranking_kpi := (1000-Divari*100 + sum_voitot)]
aggr_to_Pakka_turnaus[, ranking := frank(-ranking_kpi, na.last = "keep"), by = Turnaus_NO]
aggr_to_Pakka_turnaus[, ranking_bottom := frank(ranking_kpi, na.last = "keep"), by = Turnaus_NO]

TurnausVoitot <- aggr_to_Pakka_turnaus[
  aggr_to_Pakka_turnaus[, .I[ranking == min(ranking)],
                        by=Turnaus_NO]$V1][ranking == 1,. (TurnausVoittaja = Pakka_ID, Turnaus_NO)]

TurnausTappiot  <- aggr_to_Pakka_turnaus[
  aggr_to_Pakka_turnaus[, .I[ranking_bottom == min(ranking_bottom)],
                        by=Turnaus_NO]$V1][ranking_bottom == 1,. (TurnausHaviaja = Pakka_ID, Turnaus_NO)]

join_voitot <- TurnausVoitot[aggr_to_turnaus, on = .(Turnaus_NO = Turnaus_NO)]
join_haviaja <- TurnausTappiot[join_voitot, on = .(Turnaus_NO = Turnaus_NO)]
join_haviaja[, TurnausVoitto := ifelse(Voittaja_sum  + Tasapeli_sum * 0.5 > (Peli_LKM_sum / 2),
                                              1,
                                              ifelse(Voittaja_sum  + Tasapeli_sum * 0.5 < (Peli_LKM_sum / 2),
                                                     0,
                                                     0.5))]

STAT_TURNAUS <- join_haviaja
