getAloittaja <- function(ADM_PELIT_input, Peli_ID_input) {
  result <- NULL
  result$Aloittaja_nr <- ADM_PELIT_input[Peli_ID == Peli_ID_input & Omistaja_ID == "L", Aloittaja]
  if(result$Aloittaja_nr == 0) {
    result$Aloittaja_NM <- "Lauri"
    result$Aloittaja_ID <- "L"
  } else {
    result$Aloittaja_NM <- "Martti"
      result$Aloittaja_ID <- "M"
  }

return(result)
} 
