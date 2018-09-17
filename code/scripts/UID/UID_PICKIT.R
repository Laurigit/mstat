#UID_PICKIT
required_data(c("STG_DIVARI", "STG_PAKAT"))
UID_PICKIT <- function(STG_DIVARI, STG_PAKAT) {
 
  ssdiv <- STG_DIVARI[1==1]
  sspakat <- STG_PAKAT[, .(Pakka_ID, Omistaja_ID, Pakka_NM)]
  joini <- ssdiv[sspakat, on = "Pakka_ID"]
  return(joini)
}
