observeEvent(input$tallenna_humala,{
  Laurin_humala=as.integer(input$slider_laurin_humala_new)
  Martin_humala=as.integer(input$slider_martin_humala_new)
  alotusaika<-as.numeric(as.ITime(now(tz="Europe/Helsinki")))
  alotuspvm<-as.numeric(as.IDate(now(tz="Europe/Helsinki")))
  new_row <- data.table(Puhallusaika = alotusaika,
                        Puhalluspvm = alotuspvm,
                        Laurin_humala = Laurin_humala,
                        Martin_humala = Martin_humala)
  print(str(new_row))
  print(str(humalaData()))
  uus_data <- rbind(humalaData(), new_row)
  kircsv(uus_data, "humala.csv", TRUE)
  print(uus_data)
})