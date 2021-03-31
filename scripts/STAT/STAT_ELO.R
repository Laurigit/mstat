# required_data(c("ADM_PELIT", "STG_PAKAT"))
# library(elo)
# sscols <- ADM_PELIT[!is.na(Voittaja), .(Pakka_ID, Vastustajan_Pakka_ID, Aloitus_DT, Voittaja, Pelit_PFI, Peli_ID, Aloittaja)][order(Aloitus_DT)]
# sscols[, Pelit_PFI := ifelse(is.na(Pelit_PFI), 0.1, Pelit_PFI)]
# #sscols[, k_val := round(30 * Pelit_PFI + 10)]
# pakka_ss <- STG_PAKAT[,. (Pakka_ID, Pakka_NM)]
# 
# kertoimet <-  c(10, 10)
# sscols_sorted <- sscols[order(Aloitus_DT)]
# 
# subset_data <- sscols_sorted[1:(round(nrow(sscols_sorted) / 1))]
# 
# sscols_sorted[sample(nrow(sscols_sorted), 900), ]
# 
# rullausdata <- subset_data[, .(Elox = 1200), by = Pakka_ID]
# subset_data[, .N]
# subset_data[, .N, by = Pakka_ID]
# rullaus <- 1
# optME <- function(kertoimet) {
#   kerroin <- kertoimet[1]
#   vakio <- kertoimet[2]
#   pakka_elot <- subset_data[, .(Elo = 1200), by = Pakka_ID]
#   #pakka_elot <- alotuselot[1 != 0, .(Pakka_ID, Elo)]
#  pakka_elot_vihu <- subset_data[, .(Elo = 1200), by = Pakka_ID]
#  rullaus <<- rullaus + 1
# #for (rullaus_for in 1:100) {
# 
#  #tee sample data
#  peli_IDT <- subset_data[, Peli_ID]
#  ss_Pelit <- sample(peli_IDT, size = length(peli_IDT) * 0.9)
#  sample_data <- subset_data[Peli_ID %in% ss_Pelit]
#  sample_data[, peli_lkm := seq_len(.N), by = Pakka_ID]
#  sample_data[, aloittaja_korjaus := ifelse(Aloittaja == 1, vakio, -vakio)]
#   for(elo_loop in 1:nrow(sample_data)) {
# 
# 
# 
#   pakka <- sample_data[elo_loop, Pakka_ID]
#   vihu <- sample_data[elo_loop, Vastustajan_Pakka_ID]
#   elo_oma <- pakka_elot[Pakka_ID == pakka, Elo] 
#   elo_vihu <- pakka_elot_vihu[Pakka_ID == vihu, Elo]
#   voittaja <- sample_data[elo_loop, Voittaja]
#   vpfi <- sample_data[elo_loop, Pelit_PFI]
#   alko <- sample_data[elo_loop, aloittaja_korjaus]
#   alku_koo <- max(30 - sample_data[elo_loop, peli_lkm], 0) / 29 * 25
#   koo <- round(kerroin * vpfi + 17) + alku_koo
# 
#   elot <- elo.calc(voittaja, elo.A = elo_oma + alko, elo.B = elo_vihu, koo)
#   sample_data[elo_loop, ':=' (elo_vanha = elo_oma,
#                          elo_uus = elot[1]- alko,
#                          elo_vihu_vanha = elo_vihu,
#                          elo_vihu_uus = elot[2])]
#   pakka_elot[Pakka_ID == pakka, Elo := elot[1]]
# 
#   pakka_elot_vihu[Pakka_ID == vihu, Elo := elot[2]]
# 
#   }
#  sample_data[, win_forecast := elo.prob(elo_vanha, elo_vihu_vanha)]
#  sample_data[, voittaja_score := ifelse(Voittaja == 0, -1, 1)]
# 
#  sample_data[, forecast_score :=  ifelse(peli_lkm > 12, voittaja_score * (win_forecast - 0.5), 0)]
# 
#   score_fit <- sample_data[, mean(forecast_score) * -1]
#   print(paste0("kerroin ", kerroin, " vakio ", vakio))
#   pakka_elot[, paste0("Elo", (rullaus)) := round(Elo)]
# 
#   pakka_elot[, Elo := NULL]
#   rullausdata <<- pakka_elot[rullausdata, on = "Pakka_ID"]
# 
# 
#   join_nimi <- pakka_ss[rullausdata, on = "Pakka_ID"]
#   sortcols <- colnames(join_nimi)
#   setorderv(join_nimi, sortcols[3], order = -1)
#   print(join_nimi)
#   print(score_fit)
#   return(score_fit)
#   }
# #}
# rs <- optim(par = c(50, 50),
#             fn = optME)
# rs <- optim(par = c(40, 49),
#             fn = optME)
# pakka_elot[, Elo := round(Elo)]
# pakka_ss <- STG_PAKAT[,. (Pakka_ID, Pakka_NM)]
# join_nimi <- pakka_ss[pakka_elot, on = "Pakka_ID"]
# join_nimi[order(-Elo)]
# 
# Max_elot <- sscols[, .(max_elo = round(max(unlist(elo_uus)))), by = Pakka_ID]
# join_max_elo_nimi <- pakka_ss[Max_elot, on = "Pakka_ID"][order(-max_elo)]
