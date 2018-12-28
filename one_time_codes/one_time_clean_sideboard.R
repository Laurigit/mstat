
required_data(c("STG_PFI", "ADM_PAKKA_COMPONENTS"), TRUE)
STG_PAKAT
STG_PFI[Pakka_ID == 22]
varijako <- ADM_PAKKA_COMPONENTS[Pakka_form_ID == 112,. (Colors, Name, Count)]
varijako[, colorClass := ifelse(nchar(Colors) > 1 | is.na(Colors), "Other", Colors)]
loopivarit <- varijako[, .N, by = colorClass][, colorClass]
for(looppi in loopivarit) {
  kierrosData <- varijako[ colorClass == looppi]
  tulostusmuoto <- kierrosData[, .(print_out = paste0(Count, " ", Name))]
  colname <- paste0("KorttiLKM=", kierrosData[, sum(Count)])
  setnames(tulostusmuoto, "print_out", colname)
  filenimi <- paste0("Martti-", looppi, ".txt")
  write.csv(tulostusmuoto, file = filenimi, quote = FALSE,
            row.names = FALSE)
}
