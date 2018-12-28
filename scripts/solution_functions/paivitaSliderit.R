


paivitaSliderit<-function(input_peli_ID,session) {
  required_data("ADM_PELIT")
  laurin_pakka<-(ADM_PELIT[Peli_ID ==  input_peli_ID & Omistaja_ID == "L" ,Pakka_ID])
  martin_pakka<-(ADM_PELIT[Peli_ID ==  input_peli_ID & Omistaja_ID == "M", Pakka_ID])
  
  updateSelectInput(session,"select_laurin_pakka",selected=  laurin_pakka)
  updateSelectInput(session,"select_martin_pakka",selected=  martin_pakka)
  
}
