#tab_decks.
output$hot_decks <- renderRHandsontable({
required_data("SRC_DIVARI")
  rhandsontable(SRC_DIVARI, useTypes = FALSE)  %>%
  hot_table(contextMenu = TRUE,
            stretchH = FALSE,
            enableComments = FALSE
            )
})

observeEvent (input$Save_decks, {
  r_data <- hot_to_r(input$hot_decks)
  message("pitäis toimia")
  kircsv(r_data, "divari.csv")

  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_DIVARI", ADM_DI_HIERARKIA, input_env = globalenv(), rewriteSaveR = FALSE)
  refresh_counter$a <- isolate(refresh_counter$a +1 )
  updateActionButton(inputId = "Save_decks", label = "UPDATED")
})
