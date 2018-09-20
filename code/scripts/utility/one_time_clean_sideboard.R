analyse <- STG_PFI[is.na(Pakka_ID), Pakka_form_ID]
varijako <- ADM_PAKKA_COMPONENTS[Pakka_form_ID == analyse,. (Colors, Name, Count)]
varijako[, colorClass := ifelse(nchar(Colors) > 1 | is.na(Colors), "Other", Colors)]
loopivarit <- varijako[, .N, by = colorClass][, colorClass]
for(looppi in loopivarit) {
  kierrosData <- varijako[ colorClass == looppi]
  tulostusmuoto <- kierrosData[, .(print_out = paste0(Count, " ", Name))]
  colname <- paste0("KorttiLKM=", kierrosData[, sum(Count)])
  setnames(tulostusmuoto, "print_out", colname)
  filenimi <- paste0("Lauri-", looppi, ".txt")
  write.csv(tulostusmuoto, file = filenimi, quote = FALSE,
            row.names = FALSE)
}
