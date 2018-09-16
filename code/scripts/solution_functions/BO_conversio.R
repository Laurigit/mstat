#STAT_PELIT_BO
#required_data("ADM_PELIT")
BO_conversio <- function(ADM_PELIT) {
  


testdata <- ADM_PELIT[1 != 0]
# testdata[, Turnaus_NO := round(Turnaus_NO / 2,0)]
# testdata[Turnaus_NO %% 2 == 0, BO_mode := 1]
# testdata[Turnaus_NO %% 2 == 0, Ottelu_NO := 1]
#aggregate peli to ottelu level
aggr_BO <- testdata[, .(sum_Voittaja = sum(Voittaja),
                                       Pakka_form_ID = max(Pakka_form_ID),
                                       Pakka_form_pct = max(Pakka_form_pct),
                                       Pelit_PFI = max(Pelit_PFI),
                                       Aloitus_DT = min(Aloitus_DT),
                                       Lopetus_DT = max(Lopetus_DT),
                                       Peli_ID = NA,
                                       Ottelu_NO = NA,
                        Aloittaja = mean(Aloittaja),
                       Mulligan = mean(Mulligan),
                        Arvosana = mean(Arvosana),
                        Vuoroarvio = mean(Vuoroarvio),
                        Kasikortit = mean(Kasikortit),
                        Landit = mean(Landit),
                        Lifet = mean(Lifet),
                        Humala = mean(Humala),
                       Pakka_form_pct = mean(Pakka_form_pct),
                       Vastustajan_Pakka_form_pct = mean(Vastustajan_Pakka_form_pct),
                       Pakka_form_ID = max(Pakka_form_ID),
                       Vastustustajan_Pakka_form_ID = max(Vastustustajan_Pakka_form_ID),
                       Hinta = mean(Hinta),
                       Kortti_lkm_manastack = max(Kortti_lkm_manastack),
                        Peli_LKM = .N),
                    by = .(Pakka_NO,
                      Vastustajan_Pakka_NO,
                           Ottelu_ID,
                           Pakka_ID,
                           Vastustajan_Pakka_ID,
                           Omistaja_ID,
                           Vastustajan_Omistaja_ID,
                           Divari,
                           Kierros,
                          BO_mode,
                           Turnaus_NO)]
aggr_BO[, ':=' (Voittaja = ifelse(sum_Voittaja / Peli_LKM > 0.5, 1, 0),
                Tasapeli = ifelse(sum_Voittaja / Peli_LKM == 0.5, 1, 0))]

aggr_BO[, ':=' (Voittaja_PFI = Voittaja * Pelit_PFI,
                Tasapeli_PFI = Tasapeli * Pelit_PFI)]

aggr_BO[, ':=' (sum_Voittaja = NULL)]

#putket
setorder(aggr_BO, Pakka_ID, Aloitus_DT)
aggr_BO[, ':=' (perakkaiset = rleid(Voittaja),
                      putki_cal = ifelse(Voittaja == 1, 1, -1))]
aggr_BO[, ':=' (Putki = cumsum(putki_cal)), by = .(perakkaiset, Pakka_ID)]

#vs_putski
setorder(aggr_BO, Pakka_ID, Vastustajan_Pakka_ID, Aloitus_DT)
aggr_BO[, ':=' (perakkaiset = rleid(Voittaja))]
aggr_BO[, ':=' (Putki_VS = cumsum(putki_cal)), by = .(perakkaiset, Pakka_ID, Vastustajan_Pakka_ID)]

#tuhoa turhat
aggr_BO[, ':=' (perakkaiset = NULL, putki_cal = NULL)]

STAT_PELIT_BO <- aggr_BO
return(STAT_PELIT_BO)
}
