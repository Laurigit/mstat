#required_data("ADM_PELIT")
#Peli_ID_input <- 899
getTilanne <- function(ADM_PELIT, Peli_ID_input) {
  ottelu <- ADM_PELIT[Peli_ID == Peli_ID_input, .N, by =Ottelu_ID][, Ottelu_ID]
  tilanne <- ADM_PELIT[Ottelu_ID == ottelu, .(Tilanne = sum(Voittaja, na.rm = TRUE)), by = Pakka_ID]
  peleja_jaljella_data <- ADM_PELIT[Ottelu_ID == ottelu & is.na(Voittaja), .(peleja_jaljella = .N )]
  Peleja_jaljella_bool <- nrow(peleja_jaljella_data) > 0
  lopputulos <- cbind(tilanne, Peleja_jaljella_bool)
  return(lopputulos)
}
