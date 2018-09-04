#funktio valmistele datasetti eri moodeilla (PFI, MA ja BO)
# input_MA <- 7
# MA <- TRUE
# PFI <- TRUE
# BO <- TRUE
#Voitto_PCT TOT
#Voitot_VS
#Voito_PCT_VS
#VOITTO_PCT_MA
#Voitto_PCT_VS_MA
#Putki
#voittoennuste
#kortti lkm manastack ja lisakortit
#pileshuffle
#required_data("ADM_PELIT")
#tulos <- Prepare_Pelit_for_stats(ADM_PELIT, MA = "NO", 1, TRUE, TRUE)

#MA is either YES, NO or VS

Prepare_Pelit_for_stats <- function(ADM_PELIT,
                                    MA = "NO",
                                    input_MA = 7,
                                    PFI = FALSE,
                                    BO = FALSE) {

dataset_orig <- ADM_PELIT

if(BO == TRUE) {
  dataset_after_bo <- BO_conversio(dataset_orig)
} else {
  dataset_orig[, ':=' (Tasapeli_PFI = 0, Tasapeli = 0)]
  dataset_after_bo <- dataset_orig
}

if(PFI == TRUE) {
  col_Voittaja <- "Voittaja_PFI"
  col_Tasapeli <- "Tasapeli_PFI"
} else {
  col_Voittaja = "Voittaja"
  col_Tasapeli = "Tasapeli"
}
setorder(setDT(dataset_after_bo),  -Aloitus_DT)
if (MA == TRUE) {stop()}
if (MA == "YES") {
  dataset_after_ma <- dataset_after_bo[!is.na(Voittaja), head(.SD, input_MA), keyby = Pakka_ID]
} else if (MA == "NO") {
  dataset_after_ma <- dataset_after_bo
} else {
  dataset_after_ma <- dataset_after_bo[!is.na(Voittaja), head(.SD, input_MA), keyby = .(Pakka_ID,
                                                                                         Vastustajan_Pakka_ID)]
}

syntax <- parse(text=paste0("Voittaja_Stat := ", col_Voittaja))
results_ds <- dataset_after_ma[, eval(syntax)]
syntax2 <- parse(text=paste0("Tasapeli_Stat := ", col_Tasapeli))
results_ds <- dataset_after_ma[, eval(syntax2)]
return(results_ds)
}

