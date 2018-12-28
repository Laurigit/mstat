# # UID_UUSI_PELI
# required_data(c("STG_PAKAT", "STG_OMISTAJA", "ADM_PELIT", "INT_PFI", "STAT_VOITTOENNUSTE"))
# required_functions(c("UID_PAKKA", "UID_PAKKA_VS", "UID_UUSI_PELI"))
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
# Peli_ID_input <- "1000"
# 
# res <-  UID_UUSI_PELI(Peli_ID_input, UID_PAKKA, UID_PAKKA_VS, STG_PAKAT, STG_OMISTAJA, ADM_PELIT,
#                       STAT_VOITTOENNUSTE,
#                       input_left_mulligan,
#                       input_right_mulligan,
#                        STAT_CURRENT_PAKKA)
# res
# res
required_functions("sort_MTG_colors")
UID_UUSI_PELI <- function(Peli_ID_input,
                          UID_PAKKA,
                          UID_PAKKA_VS,
                          STG_PAKAT,
                          STG_OMISTAJA,
                          ADM_PELIT,
                          STAT_VOITTOENNUSTE,
                          input_left_mulligan,
                          input_right_mulligan,
                          STAT_CURRENT_PAKKA) {
  pelidata <- ADM_PELIT[1==1]
  
#message(Peli_ID_input, 
#          input_left_mulligan,
#          input_right_mulligan)
  required_functions("predict_result")
Left_pakka <- pelidata[Peli_ID == Peli_ID_input & Omistaja_ID=="L", .(Pakka_ID)]
Right_pakka <- pelidata[Peli_ID == Peli_ID_input & Omistaja_ID=="M", .(Pakka_ID)]
Divari <- pelidata[Peli_ID == Peli_ID_input & Omistaja_ID=="M", .(Divari)]
  
Pakka <- UID_PAKKA[Pakka_ID %in% c(Left_pakka,
                                      Right_pakka)]
PakkaVS <- UID_PAKKA_VS[Pakka_ID %in% c(Left_pakka,
                                      Right_pakka) & Vastustajan_Pakka_ID %in% c(Left_pakka,
                                                                                             Right_pakka)]
#message("PakkaVS ", PakkaVS)
Tilanne <- getTilanne(pelidata, Peli_ID_input)
Aloittaja <- pelidata[Peli_ID == Peli_ID_input , .(Pakka_ID, Aloittaja)]
sscols_pakat_temp <- STG_PAKAT[, .(Pakka_ID, Pakka_NM, Omistaja_ID)]
#join dybamic color
ssColor <- STAT_CURRENT_PAKKA[, .(Pakka_ID, Colors, Most_same_card)]
sscols_pakat <- sscols_pakat_temp[ssColor, on = "Pakka_ID"]
sscols_pakat[, Pakka_NM:= paste(word(Pakka_NM, 1, sep = "_"),
                                sort_MTG_colors(Colors),
                                word(Pakka_NM, -1, sep = "_"),
                                sep = "_"),
             by = Pakka_ID]

sscols_pakat[, Pakka_NM_no_color:= paste(word(Pakka_NM, 1, sep = "_"),
                                word(Pakka_NM, -1, sep = "_"),
                                sep = "_"),
             by = Pakka_ID]
sscols_pakat[, Pakka_color:= sort_MTG_colors(Colors),
             by = Pakka_ID]

sscols_pakat[, Colors := NULL]
joini <- PakkaVS[Tilanne,
                   on ="Pakka_ID"][Pakka,
                 on = "Pakka_ID"][Aloittaja,
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
#print("UID_UUSI_PELI <- function")
#print(paste(Peli_ID_input, input_left_mulligan,
#            input_right_mulligan
#            ))
ennuste <- predict_result(Peli_ID_input, input_left_mulligan,
               input_right_mulligan,
               STAT_VOITTOENNUSTE)
ennuste_vector <- c(ennuste, 1 - ennuste)
ennuste_cols <- data.table(Prediction = ennuste_vector)

join_ennuste <- cbind(joini_ssrows, ennuste_cols, Divari)
UID_UUSI_PELI <- join_ennuste
return(UID_UUSI_PELI)
}
