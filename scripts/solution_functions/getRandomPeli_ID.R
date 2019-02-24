#required_data("ADM_PELIT")
# input_Divari = "All"
getRandomPeli_ID <- function(ADM_PELIT, input_Divari = "All") {

  peliData <- ADM_PELIT[1 != 0]

  pelaamattomat_ottelut <- peliData[is.na(Voittaja) , .(Min_Peli_ID = min(Peli_ID)) , by = .(Ottelu_ID, Divari)]

  if(!input_Divari == "All") {
    pelaamattomat_ottelut_div <- pelaamattomat_ottelut[Divari == input_Divari]
  } else {
    pelaamattomat_ottelut_div <- pelaamattomat_ottelut
  }
  if (nrow(pelaamattomat_ottelut_div) > 1) {
   result <- base::sample(pelaamattomat_ottelut_div[,Min_Peli_ID], 1)
  } else {
    result <- pelaamattomat_ottelut_div[,Min_Peli_ID]
  }

  return(result)
}
