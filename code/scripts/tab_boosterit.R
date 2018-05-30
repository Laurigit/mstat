#tab_boosterit
output$txt_vect_boosters <- renderText({
  suggest_boosters(input$numeric_count_boosters)
})

confirmDrafted <- reactiveValues(text = "Not yet drafted")

observeEvent(input$action_add_boosters, {
  booster_vect <- suggest_boosters(input$numeric_count_boosters)
  draft_boosters(booster_vect)
  confirmDrafted$text <- booster_vect
  
})

output$txt_confirm_drafted <- renderText({
 paste(confirmDrafted$text)
 
})