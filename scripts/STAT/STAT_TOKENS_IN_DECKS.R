#STAT_TOKENS_IN_DECKS
required_data(c("STG_TOKENS", "STAT_CURRENT_PAKKA_COMPONENTS"))
#ÄLÄ JÄTÄ PÄÄLLE TÄTÄ required_data(c("STG_MANASTACK_CARDS"))
#get cards in decks and drafts

ss_pakka <- STAT_CURRENT_PAKKA_COMPONENTS[, .(Pakka_NM, Omistaja_ID, Name)]
kortit <- data.table(dbSelectAll("CARDS_DIM_with_set", con))[, .(Set, Name)]
ss_cards_no_set <- STG_MANASTACK_CARDS[, .N, by = .(Name)][, .(Name)]
joinset <- ss_cards_no_set[kortit, on = .(Name)]#
joinset[, tokenset := paste0("t", Set)]

join_tokens_and_set <- STG_TOKENS[joinset, on = .(Name, tokenset)][!is.na(Token) & Token != "Copy"]
#join_tokens_and_set[, found_in_set := Set %in% c("mma", "mm2", "mm3", "uma", "ema", "m19", "m20", "ima", "mh1", "a25", "mh2", "2xm", "2x2", "m21", "dmr", "tsr")]
#join_tokens_and_set[, count_in_sets := sum(found_in_set), by = Name]
print_us <- join_tokens_and_set[, .N, by = .(Token, uniqueness, tokenset, number)][order(Token)]
one_row_per_group <- print_us[, head(.SD, 1), by = .(Token, uniqueness)]
one_row_per_group
#print_us_final <- print_us[, .N, by = .(one_set, Token, uniqueness)]
cat((paste0(one_row_per_group[, .(paste0("1 ", Token, " (", tokenset, ") ",  number, collapse = "\n"))])))


all_tokens <- STG_TOKENS[kortit, on = "Name"][!is.na(Token) & Token != "Copy", .(countti = .N, setti = paste0(Set, collapse = ";")), by = .(Token)][order(-countti)]
all_tokens[, one_set := word(setti, 1,1, sep = ";")]
cat(print(paste0(all_tokens[, .(paste0("1 ", Token, " (T", one_set, ")", collapse = "\n"))]), row.names = FALSE))
STAT_TOKENS_IN_DECKS <- STG_TOKENS[ss_pakka, on = "Name"][!is.na(Token) & Token != "Copy", .(Token, Pakka_NM, Omistaja_ID)]
#mitkä toksut on vai laurilla. Vain martilla, molemmilla?
#mitkä toksut on pakassa X?


STG_TOKENS[, .N, by = Token]
