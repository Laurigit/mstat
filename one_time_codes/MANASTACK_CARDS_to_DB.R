fixed <- STG_MANASTACK_CARDS
fixed[, Text := gsub("Æ", "Ae", Text)]
required_data("ADM_PAKKA_COMPONENTS")
type_sss <- ADM_PAKKA_COMPONENTS[, .N, by = .(Type = Type_exact, Name)]
joinType <- type_sss[fixed, on = "Name"]
joinType[, N := NULL]

# 
# con <- connDB(con)
# cards <- dbSelectAll("CARDS_DIM", con)
# 
# get_col_order <- colnames(getCard_from_SF("Abzan Falconer"))
# cards[is.na(Type), Type := getCard_from_SF(Name)$Type, by = Name]
# 
# cards[, Type := iconv(x = Type, to = "UTF-8")]
# cards[, Text := iconv(x = Text, to = "UTF-8")]
#   
# #fixed[1:20, Name]
#Encoding(fixed[, Text]) <- "UTF-8"
#dbSendQuery(con, 'set character set "utf8"')
#clear old
#dbQ("DELETE FROM CARDS_DIM WHERE 1 = 1", con)
#dbWriteTable(con, "CARDS_DIM", joinType, append = TRUE, row.names = FALSE, fileEncoding = "UTF-8", overwrite = FALSE)
