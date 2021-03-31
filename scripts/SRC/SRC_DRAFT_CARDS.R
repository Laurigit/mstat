#SRC_DRAFT_CARDS
con <- connDB(con)
SRC_DRAFT_CARDS <- dbSelectAll("DRAFT_CARDS", con)
