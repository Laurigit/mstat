paivitaSliderit <- function(input_peli_ID,session) {
  required_data("ADM_PELIT")
  pelidata <- ADM_PELIT[1 != 0]
  #data.table bugged and deeded to filter in steps.
 pelirivit <- pelidata[Peli_ID == input_peli_ID]
  laurin_pakka <- (pelirivit[Omistaja_ID == "L" ,Pakka_ID])
  martin_pakka <- (pelirivit[Omistaja_ID == "M", Pakka_ID])
  updateSelectInput(session,"select_laurin_pakka",selected =  laurin_pakka)
  updateSelectInput(session,"select_martin_pakka",selected =  martin_pakka)
}
