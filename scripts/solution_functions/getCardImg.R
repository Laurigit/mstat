
#cardNameInput <- "Æthersnipe"
getCardImg <- function(cardNameInput) {
  #check if exists
  fixedName <- stringi::stri_trans_general(cardNameInput, "Latin-ASCII")
if (!file.exists(paste0("../common_data/", cardNameInput, ".jpg"))) {
    
  
  #cardNameInput <- "Vampire Aristocrat"
  urlName <- gsub(" ", "+", x = cardNameInput)
  url <- paste0("https://api.scryfall.com/cards/named?exact=", urlName)
  raw.result <- GET(url = url)
  result_json <- fromJSON(rawToChar(raw.result$content))
  image_url <- result_json$image_uris$art_crop

  download.file(url = image_url, destfile = paste0("../common_data/", cardNameInput, ".jpg"), mode = "wb")
}
  if (!file.exists(paste0("./www/", fixedName, ".jpg"))) {
    file.copy(from = paste0("../common_data/", cardNameInput, ".jpg"),
              to = paste0("./www/", cardNameInput, ".jpg"))
    file.copy(from = paste0("./www/", cardNameInput, ".jpg"),
              to = paste0("./www/", fixedName, ".jpg"))
  }
}


