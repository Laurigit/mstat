#func jatka_ottelua
find_jatka_ottelua <- function(ADM_PELIT) {

  aggr <- ADM_PELIT[, .(rivi_lkm = .N, vsum = sum (!is.na(Voittaja), na.rm =TRUE)), by = Ottelu_ID]
  keskenpeli <- aggr[vsum < rivi_lkm & vsum > 0, min(Ottelu_ID)]
  if(!is.infinite(keskenpeli)) {
    result_Peli_ID <- ADM_PELIT[Ottelu_ID == keskenpeli & is.na(Voittaja), min(Peli_ID)]
  } else {
    result_Peli_ID <- NA
  }
  return(result_Peli_ID)
}
