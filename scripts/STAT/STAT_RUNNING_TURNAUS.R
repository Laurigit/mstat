#STAT_RUNNING_TURNAUS
required_data("ADM_PELIT")
required_functions("NO_conversio")
#sovittu, että laskenta alkaa vasta tästä turnauksestsa
subset_pelit <- ADM_PELIT[Turnaus_NO > 53]

BO_data <- BO_conversio(subset_pelit)
# aggr_to_turnaus <- BO_data[!is.na(Aloitus_DT),.(Pakka_ID_list = list(unique(Pakka_ID)),
#                                 #Vasustajan_ID_list = list(Vastustajan_Pakka_ID),
#                                 Aloitus_DT = min(Aloitus_DT),
#                                 Lopetus_DT = max(Lopetus_DT),
#                                 Voittaja_sum = sum(Voittaja * Peli_LKM),
#                                 Peli_LKM_sum = sum(Peli_LKM, na.rm = TRUE),
#                                 Tasapeli_sum = sum(Tasapeli * Peli_LKM)
#                                 ),
#                            by = .(Turnaus_NO, Omistaja_ID)]

sorted <- BO_data[order(Aloitus_DT)][, .(Turnaus_NO, Omistaja_ID, Kierros, BO_mode, Aloitus_DT, Lopetus_DT, Peli_LKM, Voittaja, Tasapeli)]
sorted[, peleja_turnauksessa := .N, by = Turnaus_NO]
L_pelit <- sorted[Omistaja_ID == "L" & !is.na(Voittaja)]
M_pelit <- sorted[Omistaja_ID == "M"& !is.na(Voittaja)]

L_cum_voitot <- 0
M_cum_voitot <- 0
L_cum_turnaus_voitot <- 0
M_cum_turnaus_voitot <- 0
tulosrivi <- 1

for (rivi in 1:nrow(L_pelit)) {
  L_cum_voitot <- L_pelit[rivi, Peli_LKM * (Voittaja + Tasapeli / 2)] + L_cum_voitot
  M_cum_voitot <- M_pelit[rivi, Peli_LKM * (Voittaja + Tasapeli / 2)] + M_cum_voitot

  #jos voitot on yli puolet peleistä, niin turnausvoitto ja laskenta alusta
  peleja <- L_pelit[rivi, peleja_turnauksessa]
  L_pelit[rivi, ':=' (running_voitot = L_cum_voitot,
                      aloitusrivi = tulosrivi)]
  M_pelit[rivi, ':=' (running_voitot = M_cum_voitot,
                      aloitusrivi = tulosrivi)]  
  
  if (L_cum_voitot > peleja / 2) {
    L_cum_turnaus_voitot <- L_cum_turnaus_voitot + 1
    L_cum_voitot <- 0
    M_cum_voitot <- 0
    tulosrivi <- 1
  } else if (M_cum_voitot > peleja / 2) {
    M_cum_turnaus_voitot <- M_cum_turnaus_voitot + 1
    L_cum_voitot <- 0
    M_cum_voitot <- 0
    tulosrivi <- 1
  } else if (L_cum_voitot + M_cum_voitot == peleja) {
    L_cum_voitot <- 0
    M_cum_voitot <- 0
    L_cum_turnaus_voitot <- L_cum_turnaus_voitot + 0.5
    M_cum_turnaus_voitot <- M_cum_turnaus_voitot + 0.5
    tulosrivi <- 1
  } else {
    tulosrivi <- 0
  }
  
  
  
  L_pelit[rivi, ':=' (running_turnaus_voitot = L_cum_turnaus_voitot,
                      tulosrivi_col = tulosrivi)]
  M_pelit[rivi, ':=' (running_turnaus_voitot = M_cum_turnaus_voitot,
                      tulosrivi_col = tulosrivi)]
}

appendaa <- rbind(L_pelit, M_pelit)[order(Aloitus_DT)]
#tilannerivi kaks vikaa riviä
appendaa[(nrow(appendaa) - 1):nrow(appendaa), tilannerivi := 1]


tarpeelliset_rivit <- appendaa[tulosrivi_col == 1 | aloitusrivi == 1 | tilannerivi == 1]
tarpeelliset_rivit[, Running_Turnaus_NO := cumsum(aloitusrivi), by = Omistaja_ID]
aggr <- tarpeelliset_rivit[, .(Aloitus_DT = min(Aloitus_DT),
                               Lopetus_DT = max(Lopetus_DT),
                               running_turnaus_voitot = max(running_turnaus_voitot),
                               running_voitot = max(running_voitot),
                               Turnaus_Valmis = max(tulosrivi_col)
                               ),
                           by = .(Omistaja_ID, Running_Turnaus_NO)]
kaanna <- dcast.data.table(aggr, formula = Turnaus_Valmis +Running_Turnaus_NO + Aloitus_DT + Lopetus_DT ~ Omistaja_ID, value.var = c("running_turnaus_voitot", "running_voitot"))
kaanna_sort <- kaanna[order(Running_Turnaus_NO)]


STAT_RUNNING_TURNAUS <- kaanna_sort


