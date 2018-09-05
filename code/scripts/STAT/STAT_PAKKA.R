#STAT_PAKKA
required_data(c("ADM_PELIT"))
required_data(c("INT_PFI"))
required_functions("Prepare_Pelit_for_stats")
required_input("input",
               c("numeric_MA_valinta", "radio_bo_mode", "radio_pfi_mode"),
               c(7, FALSE, FALSE))

#Voitto_PCT
Voitto_PCT_data <- Prepare_Pelit_for_stats(ADM_PELIT,
                        MA = "NO",
                        BO = input$radio_bo_mode,
                        PFI = input$radio_pfi_mode)
Voitto_PCT_pakka <- Voitto_PCT_data[!is.na(Voittaja),
                               .(Voitto_PCT = mean(Voittaja_Stat, na.rm = TRUE)),
                               by = Pakka_ID]


#Voitto_PCT_MA
Voitto_PCT_MA_data <- Prepare_Pelit_for_stats(ADM_PELIT,
                                              MA = "YES",
                                              input_MA = input$numeric_MA_valinta,
                                              BO = input$radio_bo_mode,
                                              PFI = input$radio_pfi_mode)
Voitto_PCT_MA_pakka <- Voitto_PCT_MA_data[,.(Voitto_PCT_MA = mean(Voittaja_Stat, na.rm = TRUE)),
                                          by = Pakka_ID]


Lisakortit <- INT_PFI[Pakka_form_ID == Current_Pakka_form_ID,. (Pakka_ID, Deck_size = Kortti_lkm_manastack,
                                                                Shuffle8 = Kortti_lkm_manastack %% 8,
                                                                Colors = Pakka_colors)]

#putki
Putki_pakka <- Voitto_PCT_data[Voitto_PCT_data[, .I[which.max(Lopetus_DT)], by=Pakka_ID]$V1][,.(Putki, Pakka_ID)]

#join all
Voitto_PCT_pakka
STAT_PAKKA <- Lisakortit[Putki_pakka,
                                  on = "Pakka_ID"][Voitto_PCT_MA_pakka,
                                                   on = "Pakka_ID"][Voitto_PCT_pakka,
                                                                    on = "Pakka_ID"]


