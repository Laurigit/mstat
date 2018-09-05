required_data(c("STG_PELIT", "STG_HUMALA", "INT_PFI", "STG_TURNAUSSAANTO"))

#join Humala
temp_hum <- STG_HUMALA
temp_hum[, ':=' (Puhallus_DT_alku = (Puhallus_DT - 60 * 30),
            Puhallus_DT_loppu = (Puhallus_DT + 60 * 30),
            Puhallus_DT_alku_copy = Puhallus_DT - 60 * 30,
            Puhallus_DT_loppu_copy = Puhallus_DT + 60 * 30
            )]

temp_pel <- STG_PELIT
temp_pel[ ,':=' (Aloitus_DT_copy = (Aloitus_DT), Lopetus_DT_copy = (Lopetus_DT))]
join_humala <- temp_hum[temp_pel, on=.( Puhallus_DT_alku <= Lopetus_DT,
                                           Puhallus_DT_loppu >= Lopetus_DT,
                                           Omistaja_ID = Omistaja_ID)
                       ]

#aggregate over humala, if blown multiple times during a game (window)
join_humala_aggr <- join_humala[, .(Humala = mean(Humala, na.rm = TRUE)), by = .(Aloitus_DT = Aloitus_DT_copy,
                                                                                 Aloitus_DT_copy,
                                                                                Lopetus_DT_copy,
                                                                                Pakka_ID,
                                                                                Vastustajan_Pakka_ID,
                                                                                Omistaja_ID,
                                                                                Vastustajan_Omistaja_ID,
                                                                                Divari,
                                                                                Pakka_NO,
                                                                                Vastustajan_Pakka_NO,
                                                                                Kierros,
                                                                                Ottelu_ID,
                                                                                Ottelu_NO,
                                                                                BO_mode,
                                                                                Turnaus_NO,
                                                                                Aloittaja,
                                                                                Peli_ID,
                                                                                Voittaja,
                                                                                Mulligan,
                                                                                Arvosana,
                                                                                Landit,
                                                                                Vuoroarvio,
                                                                                Kasikortit,
                                                                                Lifet)]

#join PFI
temp_pfi <- INT_PFI[, .(Pakka_ID, Valid_from_DT, Valid_to_DT, Pakka_form_ID, Pakka_form_pct, Hinta, Kortti_lkm_manastack)]
join_humala_pfi <- temp_pfi[join_humala_aggr, on = .(Pakka_ID,
                                                     Valid_from_DT <= Aloitus_DT,
                                                     Valid_to_DT >= Aloitus_DT)]
join_humala_pfi_sscols <- join_humala_pfi[,. (Vastustajan_Pakka_form_pct = Pakka_form_pct,
                                              Vastustustajan_Pakka_form_ID = Pakka_form_ID,Peli_ID,
                                              Vastustajan_Pakka_ID = Pakka_ID)]
#join vihun PFI

molemmat_pfit <- join_humala_pfi_sscols[join_humala_pfi, on = .(Peli_ID, Vastustajan_Pakka_ID)]


molemmat_pfit[,  ':=' (Voittaja_PFI = Pakka_form_pct * Vastustajan_Pakka_form_pct * Voittaja,
                       Pelit_PFI = Pakka_form_pct * Vastustajan_Pakka_form_pct,
                       Aloitus_DT = Aloitus_DT_copy,
                       Lopetus_DT = Lopetus_DT_copy,
                       Valid_to_DT = NULL,
                       Valid_from_DT = NULL,
                       Aloitus_DT_copy = NULL,
                       Lopetus_DT_copy = NULL
                       
                       )]
molemmat_pfit[, ':=' (Peli_LKM = 1)]

#putket
setorder(molemmat_pfit, Pakka_ID, Aloitus_DT)
molemmat_pfit[, ':=' (perakkaiset = rleid(Voittaja),
             putki_cal = ifelse(Voittaja == 1, 1, -1))]
molemmat_pfit[, ':=' (Putki = cumsum(putki_cal)), by = .(perakkaiset, Pakka_ID)]

#vs_putski
setorder(molemmat_pfit, Pakka_ID, Vastustajan_Pakka_ID, Aloitus_DT)
molemmat_pfit[, ':=' (perakkaiset = rleid(Voittaja))]
molemmat_pfit[, ':=' (Putki_VS = cumsum(putki_cal)), by = .(perakkaiset, Pakka_ID, Vastustajan_Pakka_ID)]

#tuhoa turhat
molemmat_pfit[, ':=' (perakkaiset = NULL, putki_cal = NULL)]



ADM_PELIT <- molemmat_pfit

