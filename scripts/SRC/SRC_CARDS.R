#SRC_CARDS
con <- connDB(con)
SRC_CARDS <- dbSelectAll("CARDS", con)
#SRC_CARDS[, Valid_from_DT := strptime(Valid_from_DT, format = "%Y-%m-%d %H:%M:%S")]
