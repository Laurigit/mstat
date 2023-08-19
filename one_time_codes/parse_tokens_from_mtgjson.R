#lataa täältä https://mtgjson.com/downloads/all-files/ AllPrintingsCSVFiles. Pura toksut. Aja tämä. committaa repoon
toksut_raw <- data.table(read.csv("../useful_files_like_tokens.csv/tokens.csv"))[, .(name, relatedCards, colorIdentity, power,  subtypes, supertypes, text, toughness, type,types, number, setCode )]
toksut_raw[10]
toksut_raw[, uniqueness := paste0(trimws(power), "/", trimws(toughness), " ", trimws(colorIdentity), " ", trimws(text))]

toksut <- toksut_raw[,.(name, relatedCards, uniqueness, number, setCode)]
toksut[, clean1 := gsub("\\{'reverseRelated': \\[|\\]\\}", "", relatedCards)]
toksut[1]
toksut[10]
toksut[, clean_uus := gsub('"', "'", clean1), by = .(name, uniqueness, number, setCode)]
toksut[1]
toksut[, clean := str_replace_all(clean_uus, "', '", ";"), by = .(name, uniqueness, number, setCode)]
toksut[1, clean_uus]
toksut[1, clean]
toksut[, clean4 := str_sub(clean, 2, -2), by = .(name, uniqueness, number, setCode)]
toksut[1, clean4]
cat(toksut[1, clean4])

korjattu <- toksut[, .(sha = trimws(unlist(strsplit(clean4, ";")))), by = c("name", "uniqueness", "number", "setCode")]
korjattu[11]
required_data("SRC_CARDS_DIM")
ss_cards <- SRC_CARDS_DIM[, .(Name)]
join_toksucads <- korjattu[ss_cards, on = .(sha = Name)]
tulos <- join_toksucads[!is.na(name), .N, by = .(sha, name, uniqueness, number, setCode)]
tulos[name == "Angel"]
PARSITUT_TOKSUT <- tulos[, .(Card_name = sha, Token = name, uniqueness, number, setCode)]
save(file = "./external_files_in_git/PARSITUT_TOKSUT.RData", PARSITUT_TOKSUT)


toksut_print <- korjattu[, .N, by = name][N < 3]

cat(print(paste0(toksut_print[, .(paste0("1 ", name, " (T", "M21", ")", collapse = "\n\r"))]), row.names = FALSE))

