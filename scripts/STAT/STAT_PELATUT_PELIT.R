#STAT_PELATUT_PELIT
required_data(c("ADM_PELIT", "STG_PAKAT"))
Pelatut <- ADM_PELIT[!is.na(Voittaja),. (Vastustajan_Pakka_ID,
                                         Turnaus_NO
                                         , Pakka_ID,
                                         Vastustajan_Pakka_form_pct,
                                         Peli_ID,
                                         Vastustajan_Kortti_lkm_manastack,
                                         Vastustajan_Hinta,
                                         Pakka_form_pct,
                                         Hinta,
                                         Kortti_lkm_manastack,
                                         Aloitus_DT,
                                         Lopetus_DT,
                                         Kesto = difftime( Aloitus_DT, Lopetus_DT, units = c("mins")),
                                         Omistaja_ID,
                                         Vastustajan_Omistaja_ID,
                                         Divari,
                                         Kierros,
                                         Ottelu_ID,
                                         Ottelu_NO,
                                         BO_mode,
                                         Aloittaja,
                                         Voittaja,
                                         Mulligan,
                                         Vastustajan_Mulligan,
                                         Mulligan_etu = Vastustajan_Mulligan - Mulligan,
                                         Arvosana,
                                         Vastustajan_Arvosana,
                                         Landit,
                                         Vastustajan_Landit,
                                         Landi_etu = Landit - Vastustajan_Landit,
                                         Vuoroarvio,
                                         Kasikortit,
                                         Vastustajan_Kasikortit,
                                         Korttietu = Kasikortit - Vastustajan_Kasikortit,
                                         Lifet,
                                         Vastustajan_Lifet,
                                         Humala,
                                         Voittaja_PFI,
                                         Pelit_PFI,
                                         Putki,
                                         Putki_VS)]
#minimissaan 10 pelia
peli_lkm <- Pelatut[, .N, by = Pakka_ID][N > 9]
tarpeeks_peleja <-Pelatut[peli_lkm, on = "Pakka_ID"]
nimet <- STG_PAKAT[, .(Pakka_ID, Pakka_NM)]
STAT_PELATUT_PELIT <- nimet[tarpeeks_peleja, on = "Pakka_ID"]
#laita se pakkanimi tänne
