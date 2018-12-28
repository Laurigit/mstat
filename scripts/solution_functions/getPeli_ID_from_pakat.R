#getPeli_ID_from_pakat
# P1 <- 3
# P2 <- 1
getPeli_ID_from_pakat <- function(P1, P2, ADM_PELIT) {
  tulos_peli_id <- ADM_PELIT[Pakka_ID == P1 & Vastustajan_Pakka_ID == P2 & is.na(Voittaja), min(Peli_ID)]
  #if infinite, then find max played Peli_ID
  if(is.infinite(tulos_peli_id)) {
    tulos_peli_id <- ADM_PELIT[Pakka_ID == P1 & Vastustajan_Pakka_ID == P2 , max(Peli_ID)]
  }
  return(tulos_peli_id)
}
