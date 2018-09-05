#UID_UUSI_PELI
required_data(c("STAT_PAKKA", "STAT_PAKKA_VS", "STG_PAKAT", "STG_OMISTAJA"))
required_input("input",
              c("selectInputLauri", "selectInputMartti"),
              c(1, 9))

Pakka <- STAT_PAKKA[Pakka_ID %in% c(input$selectInputLauri,
                                      input$selectInputMartti)]
PakkaVS <- STAT_PAKKA_VS[Pakka_ID %in% c(input$selectInputLauri,
                                      input$selectInputMartti) & Vastustajan_Pakka_ID %in% c(input$selectInputLauri,
                                                                                             input$selectInputMartti)]
sscols_pakat <- STG_PAKAT[, .(Pakka_ID, Pakka_NM, Omistaja_ID)]
joini <- sscols_pakat[STG_OMISTAJA, on = "Omistaja_ID"][Pakka, on = "Pakka_ID"][PakkaVS, on ="Pakka_ID"]
joini[, ':=' (Pakka_ID = NULL,
              Omistaja_ID = NULL,
              Vastustajan_Pakka_ID = NULL,
              Colors = NULL)]
UID_UUSI_PELI <- joini
