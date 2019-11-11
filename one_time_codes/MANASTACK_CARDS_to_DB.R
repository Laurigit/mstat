#fixed <- MANASTACK_CARDS
#fixed[, Text := str_replace_all(Text, "[^[:alnum:]]", " ") ]

#Encoding(fixed[, Text]) <- "UTF-8"
#dbSendQuery(con, 'set character set "utf8"')
#dbWriteTable(con, "CARDS_DIM", MANASTACK_CARDS, append = TRUE, row.names = FALSE, fileEncoding = "UTF-8")
