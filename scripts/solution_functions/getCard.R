
getCard <- function(mana_stack_id) {
  #mana_stack_id <- 68173
  url  <- paste0("http://api.manastack.com/card?id=",mana_stack_id)
  path <- "card"
  raw.result <- GET(url = url, path = "card")
  result_json <- fromJSON(rawToChar(raw.result$content))
  # names(result_json)
  # names(STG_PAKKA_COMPONENTS)

  result_row <- data.table( Card_ID = result_json$id,
                            Name = result_json$name,
                            Text = ifelse(is.null(result_json$text), NA,  result_json$text),
                            Cost = ifelse(is.null(result_json$mana_cost), NA, result_json$mana_cost),
                            Converted_Cost = ifelse(is.null(result_json$converted_cost), NA, result_json$converted_cost),
                            Rarity = result_json$rarity,
                            Colors = ifelse(is.null(result_json$colors), NA, result_json$colors),
                            Stats = ifelse(is.null(result_json$stats), NA, result_json$stats))
 
  
  return(result_row)
}
