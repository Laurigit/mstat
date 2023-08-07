#STG_TOKENS
required_data("SRC_TOKENS")
STG_TOKENS <- copy(SRC_TOKENS[,.(Name = Card_name, Token)])
