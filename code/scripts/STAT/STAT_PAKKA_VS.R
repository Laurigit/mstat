#STAT_PAKKA_VS
required_data(c("ADM_PELIT"))
required_data(c("INT_PFI"))
required_functions("Prepare_Pelit_for_stats")
required_input("input",
               c("numeric_MA_valinta", "radio_bo_mode", "radio_pfi_mode"),
               c(7, FALSE, FALSE))


#Voitto_PCT_VS_MA
Voitto_PCT_MA_VS_data <- Prepare_Pelit_for_stats(ADM_PELIT,
                                                 MA = "VS",
                                                 input_MA = input$numeric_MA_valinta,
                                                 BO = input$radio_bo_mode,
                                                 PFI = input$radio_pfi_mode)
Voitto_PCT_MA_VS_pakka <- Voitto_PCT_MA_VS_data[,.(Voitto_PCT_MA_VS = mean(Voittaja_Stat, na.rm = TRUE)),
                                                by = .(Pakka_ID, Vastustajan_Pakka_ID)]

#Voitto_PCT
Voitto_PCT_data <- Prepare_Pelit_for_stats(ADM_PELIT,
                                           MA = "NO",
                                           BO = input$radio_bo_mode,
                                           PFI = input$radio_pfi_mode)

Voitto_PCT_VS_pakka <- Voitto_PCT_data[!is.na(Voittaja),
                                       .(Voitto_PCT_VS = mean(Voittaja_Stat, na.rm = TRUE),
                                         Pelit_ABS_VS = sum(Peli_LKM, na.rm = TRUE)),
                                       by = .(Pakka_ID, Vastustajan_Pakka_ID)]


#putki_VS
Putki_pakka_VS <-
  Voitto_PCT_data[Voitto_PCT_data[,
                                                  .I[which.max(Lopetus_DT)], by=.(Pakka_ID,
                                                                                    Vastustajan_Pakka_ID)]$V1][,.(Putki_VS,
                                                                                                                  Pakka_ID,
                                                                                                Vastustajan_Pakka_ID)]

STAT_PAKKA_VS <-
  Voitto_PCT_MA_VS_pakka[Voitto_PCT_VS_pakka,
                           on = .(Pakka_ID, Vastustajan_Pakka_ID)][Putki_pakka_VS,
                                              on = .(Pakka_ID, Vastustajan_Pakka_ID)]
