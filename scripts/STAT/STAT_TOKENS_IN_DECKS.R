#STAT_TOKENS_IN_DECKS
required_data(c("STG_TOKENS", "STAT_CURRENT_PAKKA_COMPONENTS"))
ss_pakka <- STAT_CURRENT_PAKKA_COMPONENTS[, .(Pakka_NM, Omistaja_ID, Name)]
STAT_TOKENS_IN_DECKS <- STG_TOKENS[ss_pakka, on = "Name"][!is.na(Token) & Token != "Copy", .(Token, Pakka_NM, Omistaja_ID)]
#mitkä toksut on vai laurilla. Vain martilla, molemmilla?
#mitkä toksut on pakassa X?
