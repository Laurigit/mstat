#cardNameInput <- "Æthersnipe"
getCardMid <- function(cardNameInput) {
  #check if exists
  fixedName <- stringi::stri_trans_general(cardNameInput, "Latin-ASCII")

   
    #cardNameInput <- "Kongming,+\"Sleeping+Dragon\""
    urlName_frst <- gsub(" ", "+", x = cardNameInput)
    urlName <- gsub("\"", "", x = urlName_frst)
      url <- paste0('https://api.scryfall.com/cards/named?exact=\'',  urlName, '\'')
    raw.result <- GET(url = url)
    result_json <- fromJSON(rawToChar(raw.result$content))
    mid <- result_json$multiverse_ids
return(mid)
}

