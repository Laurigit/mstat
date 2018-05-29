#render_deck_evolve_plot
output$win_distribution <- renderPlot({
  all_data <- modelHistoryDataReact()
  
# input <- NULL
# r_valittu_peli <- NULL
# r_valittu_peli$aloittaja <- 0
# input$select_laurin_pakka <-4
# input$select_martin_pakka <-8
  
ss_cols <- all_data[Omistaja == "Martti" &
                      Pakka == input$select_martin_pakka &
                      Vastustajan_Pakka == input$select_laurin_pakka &
                      Aloittaja == r_valittu_peli$aloittaja,
                    .(                      
                                            VS_vaikutus,
                                            Aloittajan_vaikutus,
                                            Pakan_vaikutus,
                                            Ennuste = VS_vaikutus + Aloittajan_vaikutus + Pakan_vaikutus,
                                            TurnausNo)]
graphs_breaks <- ss_cols[, TurnausNo]

melttaa <- melt(ss_cols, id.vars = "TurnausNo", measure.vars = c("VS_vaikutus", "Aloittajan_vaikutus",
                                                                 "Pakan_vaikutus",
                                                                 "Ennuste"))
melttaa[, Martin_etu := value]

plot <- ggplot(melttaa, aes(x = TurnausNo, y = Martin_etu, colour = variable)) + geom_line(size = 1.5) +
  theme_calc() + scale_color_calc() 

plot+ theme(legend.title=element_blank(),
            legend.position = c(0.12, 0.1),
            legend.background = element_rect(color = "black",
                                             fill = "transparent", size = 1, linetype = "solid")) +
  scale_x_continuous(name = "TurnausNo",
                     breaks = graphs_breaks) +
  ylim(-0.4,0.4)



})