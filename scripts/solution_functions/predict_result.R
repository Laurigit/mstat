# input_Peli_ID<-900
# required_data("STAT_VOITTOENNUSTE")
# Laurin_Mulligan <- 0
# Martin_Mulligan <- 0

predict_result <- function(input_Peli_ID, Laurin_Mulligan = 0, Martin_Mulligan = 0, STAT_VOITTOENNUSTE) {

  Laurin_Mulligan <- ifelse(is.null(Laurin_Mulligan), 0, Laurin_Mulligan)
  Martin_Mulligan <- ifelse(is.null(Martin_Mulligan), 0, Martin_Mulligan)
  # print("Predict")
  # print(input_Peli_ID)
  # print(STAT_VOITTOENNUSTE[Peli_ID == input_Peli_ID])
  pelidata <- cbind(STAT_VOITTOENNUSTE[Peli_ID == input_Peli_ID & Omistaja_ID == "L"], Mull_diff =  Martin_Mulligan - Laurin_Mulligan,
                    VS_peli_bool = 1)
  pelidata[, ennuste := predict.glm(object =malli[[1]], newdata = .SD, type = "response")]
  return(pelidata[, ennuste])
}
# for ( looppi in 800:810) {
# tulos <- predict_result(looppi, 0, 1, STAT_VOITTOENNUSTE)

# }
