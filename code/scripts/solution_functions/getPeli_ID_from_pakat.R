#getPeli_ID_from_pakat
getPeli_ID_from_pakat <- function(P1, P2, ADM_PELIT) {
  tulos_peli_id <- ADM_PELIT[Pakka_ID == P1 & Vastustajan_Pakka_ID == P2 & is.na(Voittaja), min(Peli_ID)]
  return(tulos_peli_id)
}
