getDeckByID <- function(Deck_names, STG_DIVARI) {
  input_file <- NULL
  for(looppi in 1:nrow(Deck_names)) {
    deck_name <- Deck_names[looppi, Deck]
  
  input_Manastack_Deck_ID <- STG_DIVARI[Deck_name %in% deck_name, Manastack_Deck_ID]
  json_prefix <- STG_DIVARI[Deck_name %in% deck_name, Json_Prefix]
  required_functions("process_uploaded_decks")
  url  <- paste0("http://api.manastack.com/deck/list?id=", input_Manastack_Deck_ID)

  raw.result <- GET(url = url)
  result_json <- rawToChar(raw.result$content)
 # result_json_JSON <- fromJSON(result_json)
  write.table(x = result_json, file = paste0("./save_deck_here_from_mtg/", json_prefix, ".json"),
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE)
    # names(result_json)
  # names(STG_PAKKA_COMPONENTS)
  input_file_row <- NULL
  input_file_row <- data.frame(name =paste0(json_prefix, ".json"),
                               datapath = paste0("./save_deck_here_from_mtg/", json_prefix, ".json"), stringsAsFactors = FALSE)
  #input_file$datapath <- paste0("./save_deck_here_from_mtg/", json_prefix, ".json")
  #input_file$name <- paste0(json_prefix, ".json")
  input_file <- rbind(input_file, input_file_row)
  }
  
  
  validointi_teksti <- process_uploaded_decks(input_file, "./externfal_files/")
  
  return(validointi_teksti) 
}
