observeEvent(input$tallenna_humala,{
  Laurin_humala=as.numeric(input$slider_laurin_humala_new)
  Martin_humala=as.numeric(input$slider_martin_humala_new)
  alotusaika<-as.numeric(as.ITime(now(tz="Europe/Helsinki")))
  alotuspvm<-as.numeric(as.IDate(now(tz="Europe/Helsinki")))
  puhallus_dt <- now(tz = "EET")
  new_row <- data.table(Puhallusaika = NA,
                        Puhalluspvm = NA,
                        Laurin_humala = Laurin_humala,
                        Martin_humala = Martin_humala,
                        Puhallus_DT = as.character(puhallus_dt))

  humalaData <- luecsv("humala.csv")
  uus_data <- rbind(humalaData, new_row)
  kircsv(uus_data, "humala.csv")
  if (session$user != "overlay") {  
  updateTabItems(session, "sidebarmenu", "tab_uusi_peli")
  }
})

