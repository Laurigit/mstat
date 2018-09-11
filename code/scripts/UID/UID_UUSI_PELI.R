# UID_UUSI_PELI
# required_data(c("STG_PAKAT", "STG_OMISTAJA", "ADM_PELIT", "INT_PFI", "STAT_VOITTOENNUSTE"))
# required_functions(c("UID_PAKKA", "UID_PAKKA_VS"))
# input_MA_length = 7
# input_BO_mode  = FALSE
# input_pfi_mode = TRUE
# input_left_mulligan <- 0
# input_right_mulligan <- 0
# UID_PAKKA <- UID_PAKKA(ADM_PELIT,
#                        INT_PFI,
#                        input_MA_length,
#                        input_BO_mode,
#                        input_pfi_mode)
# 
# UID_PAKKA_VS <- UID_PAKKA_VS(ADM_PELIT, INT_PFI,input_MA_length,
#              input_BO_mode,
#              input_pfi_mode)
# 
# Peli_ID_input <- 800
# 
# res <-  UID_UUSI_PELI(Peli_ID_input, UID_PAKKA, UID_PAKKA_VS, STG_PAKAT, STG_OMISTAJA, ADM_PELIT,
#                       STAT_VOITTOENNUSTE,
#                       input_left_mulligan,
#                       input_right_mulligan)
# res
# res
UID_UUSI_PELI <- function(Peli_ID_input, UID_PAKKA, UID_PAKKA_VS, STG_PAKAT, STG_OMISTAJA, ADM_PELIT, STAT_VOITTOENNUSTE,
                          input_left_mulligan,
                          input_right_mulligan) {
  required_functions("predict_result")
Left_pakka <- ADM_PELIT[Peli_ID == Peli_ID_input & Omistaja_ID=="L", .(Pakka_ID)]
Right_pakka <- ADM_PELIT[Peli_ID == Peli_ID_input & Omistaja_ID=="M", .(Pakka_ID)]
  
Pakka <- UID_PAKKA[Pakka_ID %in% c(Left_pakka,
                                      Right_pakka)]
PakkaVS <- UID_PAKKA_VS[Pakka_ID %in% c(Left_pakka,
                                      Right_pakka) & Vastustajan_Pakka_ID %in% c(Left_pakka,
                                                                                             Right_pakka)]

Tilanne <- getTilanne(ADM_PELIT, Peli_ID_input)
Aloittaja <- ADM_PELIT[Peli_ID == Peli_ID_input , .(Pakka_ID, Aloittaja)]
sscols_pakat <- STG_PAKAT[, .(Pakka_ID, Pakka_NM, Omistaja_ID)]
joini <- Tilanne[Pakka,
                 on = "Pakka_ID"][Aloittaja,
                                  on ="Pakka_ID"][PakkaVS,
                                                  on ="Pakka_ID"][sscols_pakat,
                                                                  on = "Pakka_ID"][STG_OMISTAJA,
                                                                                   on = "Omistaja_ID"]
joini_ssrows <- joini[Pakka_ID %in%  c(Left_pakka,
                                       Right_pakka)]
joini_ssrows[, ':=' (Pakka_ID = NULL,
              Omistaja_ID = NULL,
              Vastustajan_Pakka_ID = NULL,
              Colors = NULL)]
#prediction
ennuste <- predict_result(Peli_ID_input, input_left_mulligan,
               input_right_mulligan,
               STAT_VOITTOENNUSTE)
ennuste_vector <- c(ennuste, 1 - ennuste)
ennuste_cols <- data.table(Prediction = ennuste_vector)

join_ennuste <- cbind(joini_ssrows, ennuste_cols)
UID_UUSI_PELI <- join_ennuste
return(UID_UUSI_PELI)
}
