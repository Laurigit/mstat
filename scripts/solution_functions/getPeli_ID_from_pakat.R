#getPeli_ID_from_pakat
# P1 <- 3
# P2 <- 1
getPeli_ID_from_pakat <- function(P1, P2, ADM_PELIT) {
  P1_num <- as.numeric(P1)
  P2_num <- as.numeric(P2)
  tulos_peli_id <- ADM_PELIT[Pakka_ID == P1_num & Vastustajan_Pakka_ID == P2_num & is.na(Voittaja), min(Peli_ID)]
  #if infinite, then find max played Peli_ID
  if(is.infinite(tulos_peli_id)) {
    tulos_peli_id <- ADM_PELIT[Pakka_ID == P1_num & Vastustajan_Pakka_ID == P2_num , max(Peli_ID)]
  }
  return(tulos_peli_id)
}
