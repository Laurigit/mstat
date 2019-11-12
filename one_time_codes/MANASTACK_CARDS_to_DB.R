fixed <- STG_MANASTACK_CARDS
fixed[, Text := gsub("Æ", "Ae", Text)]
#fixed[1:20, Name]
#Encoding(fixed[, Text]) <- "UTF-8"
#dbSendQuery(con, 'set character set "utf8"')
#dbWriteTable(con, "CARDS_DIM", fixed, append = FALSE, row.names = FALSE, fileEncoding = "UTF-8", overwrite = TRUE)
