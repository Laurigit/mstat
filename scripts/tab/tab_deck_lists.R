#tab_deck_lists
required_data("STG_PAKAT")
output$linklist <- renderUI({
  urlit <- STG_PAKAT[Picked == 1, .(Pakka_NM, url = gsub("_", "-", tolower(paste0("https://manastack.com/deck/", Manastack_name_url))))]
  urlit[, url_html := paste0('<a href="',url, '"  target="_blank">',Pakka_NM, '</a>')]
 

  
  
  box(HTML( paste(urlit[, url_html], collapse = "<br>")))
})
