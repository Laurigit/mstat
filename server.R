
################
#Life counter data
#turn = TSID, Atual trn = turn in magic game.
turnData <- reactiveValues(turn = 1)

required_data("ADM_DI_HIERARKIA")
updateData("SRC_CURRENT_DMG", ADM_DI_HIERARKIA, globalenv())
required_data("ADM_CURRENT_DMG")
life_totals <- reactiveValues(data = calc_life_totals(ADM_CURRENT_DMG))
damage_data <- reactiveValues(data = ADM_CURRENT_DMG)
input_error <- reactiveValues(error = FALSE)
input_error_response <- reactiveValues(response = NULL)

################GLOBAL UI control
slider_laurin_mulligan <-  reactiveValues(value = 0) 
slider_martin_mulligan <-  reactiveValues(value = 0) 
slider_laurin_virhe <-  reactiveValues(value = 1) 
slider_martin_virhe <-  reactiveValues(value = 1) 
slider_laurin_landit <-  reactiveValues(value = 0) 
slider_martin_landit <-  reactiveValues(value = 0) 
slider_laurin_lifet <-  reactiveValues(value = 0) 
slider_martin_lifet <-  reactiveValues(value = 0)
slider_vuoroarvio <-  reactiveValues(value = 0) 
slider_laurin_kasikortit <-  reactiveValues(value = -1) 
slider_martin_kasikorit <-  reactiveValues(value = -1) 
tallenna_tulos_ui_update <-  reactiveValues(value = 0) 
start_life_counter_button <- reactiveValues(value = 0)


select_laurin_pakka <- reactiveValues(value = NULL) 
select_martin_pakka <- reactiveValues(value = NULL) 
react_lauri_voitti<- reactiveValues(value = 0)
react_martti_voitti <- reactiveValues(value = 0)
###############

user_logged <- reactiveValues(count = 0)


shinyServer(function(input, output, session) {
  #load_scripts.R
 # print(session$clientData)
 
  
func_login <- function(input_user_count, clientDataInput) {
  cdata <- clientDataInput
    login <- cdata[["url_search"]]

    nimi <- word(login, 2, sep = "=")
    print("nimi")
    print(login)
    print(nimi)
    if (login == "") {
        if (input_user_count == 1) {
        result <- "Lauri"    
      } else {
        result <- "Martti"    
      }
    } else {
    result <- nimi
  }
  return(result)
}
#user_logged$count <- user_logged$count + 1
isolate(user_logged$count <- user_logged$count + 1)
session$user <- isolate(func_login(user_logged$count, session$clientData))



load_data_from_DB()
  
  sourcelist <- data.table(polku = c(dir("./scripts/", recursive = TRUE)))
  sourcelist[, rivi := seq_len(.N)]
  suppressWarnings(sourcelist[, kansio := strsplit(polku, split = "/")[1], by = rivi])
  sourcelist <- sourcelist[!grep("load_scripts.R", polku)]
  sourcelist[, kansio := ifelse(str_sub(kansio, -2, -1) == ".R", "root", kansio)]

  input_kansio_list <- c(
                         "tabstatic",
                         "tab",
                         "root"
                         )
  for(input_kansio in input_kansio_list) {
    dir_list <- sourcelist[kansio == input_kansio, polku]
    for(filename in dir_list) {
      result = tryCatch({
        print(paste0("sourced ", filename))
        source(paste0("./scripts/", filename), local = TRUE)
      }, error = function(e) {
        print(paste0("error in loading file: ", filename))
      })
    }
  }
  


  
 # load("./external_files/tilastoAsetukset.R")
  #load("./external_files/saavutusAsetukset.R")


 #write.table(x = saavutusAsetukset[,.(kuvaus, minVaiMax, Esitysmuoto, Palkintonimi)], file = "saavutusAsetukset.csv", sep = ";")



  required_data("STAT_VOITTOENNUSTE", saveR = TRUE)
 
  # 
  # sourcelist <- dir("./scripts/")
  # tab_sources <- sourcelist[grepl("tab", sourcelist)]
  # 
  # 
  # for(filename in tab_sources) {
  #   source(paste0("./scripts/", filename), local = TRUE)
  # }

  #write shiny env name
  shiny_env <- environment()
  save(shiny_env, "shiny_env", file = "./shiny_env.R")
  
   #obserEventit
  refresh_counter <- reactiveValues(a = 0)
  observeEvent(input$refresh,{
    refresh_counter$a <- refresh_counter$a + 1  
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

    #nollaa temp data
    observeEvent(input$nollaa_temp_data, {
      tyhjataulu<-data.table(muuttuja=c("kesken","laheta"),arvo=c("FALSE","FALSE"))
     # print("tässä lähetetään tyhjataulu pilveen.")
      tyhjataulu
      kircsv(tyhjataulu,"./temp_data_storage.csv", upload = TRUE)
      
    })

  #päivitä divarit
  observeEvent(input$tallenna_divarit,{
 
    
    divarit<-divaridata()
    
    
    divarit[,syntax:=(text=paste0(Pakka,Omistaja))]
    
    lapply(divarit[,syntax],function(i) {
      divarit[syntax==i,Divari:=input[[i]]]
      
    })
    divarit[,syntax:=NULL]
    print(divarit)
    kircsv(divarit,"./divari.csv")
    #divaridata<-divarit
  
  })
  

 
  #divaricheckbox
  output$checkboxPakat<-renderUI({
    divarit<-divaridata()
    lapply(divarit[,rivi_id], function(i) {
      
      checkboxInput(paste0("checkbox", divarit[rivi_id==i,Pakka],divarit[rivi_id==i,Omistaja]),label=paste0(divarit[rivi_id==i,Nimi]),value=divarit[rivi_id==i,Picked])
   
    })
  })


  observeEvent(input$sidebarmenu,{
    required_data("STAT_TURNAUS")
    maxturnaus <-max(STAT_TURNAUS[,Turnaus_NO])
    updateNumericInput(session,"sarjataulukkokierros",value=maxturnaus)})


  # divaridata <- reactiveFileReader(2000, session, "divari.csv",luecsv)
  divaridata <- reactive({
  
    tulos <- luecsv("./divari.csv")
    print(paste(input$tallenna_bannit))

    return(tulos)
  })

humalaData <- reactive({
  tulos <- luecsv("./humala.csv")
  print(input$tallenna_humala)
  return(tulos)
})
  defaultStatValue<-reactiveValues(
   
  asetukset=list("Nimi","Vastustajan Nimi","Voitti",list(),"Average","Table")

  
  )
  
    
   #luo tilasto-asetus-objekti
   

   
   ennusteDataReact <- eventReactive(input$luo_peleja, {
     create_forecast_data_for_stats(peliData_ja_pfi_react(), divaridata())
   }, ignoreNULL = FALSE)

   modelHistoryDataReact <- eventReactive(input$luo_peleja, {
     if (input$luo_peleja > 0) {
       model_history_data_new <- create_data_for_win_disribution() 
       saveR_and_send(model_history_data,"model_history_data","model_history_data.R")
     } else {
       model_history_data_new <- model_history_data
     }
     return(model_history_data_new)
   }, ignoreNULL = FALSE, ignoreInit = FALSE)

   observeEvent(input$myPivotData,{
       #ota edelliset asetukset talteen
       cnames <- list("cols","rows","vals", "exclusions","aggregatorName", "rendererName")
       # Apply a function to all keys, to get corresponding values
       allvalues <- lapply(cnames, function(name) {
         item <- input$myPivotData[[name]]
         
       })
       defaultStatValue$asetukset<-allvalues
     })


observe({
  req(input$file1)
  print(paste("ifile"))
  ifile <-input$file1
  print(ifile)
 # omistaja <- substr(1,1,ifile$name)
  if (!is.null(ifile)) {
    validointiteksti$teksti<-process_uploaded_decks(ifile,".//")}
  zip_all_and_send()
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_PFI", ADM_DI_HIERARKIA, input_env = globalenv(), rewriteSaveR = FALSE)
  refresh_counter$a <- isolate(refresh_counter$a +1 )
})





ennusteMallitReact <- eventReactive(input$luo_peleja,{
  voittoEnnusteMallit(peliData_ja_pfi_react())
}, ignoreNULL = FALSE)



validointiteksti <-reactiveValues(teksti="Ei ladattu pakkoja")
output$text_validointi <- renderText(({
  paste(validointiteksti$teksti)
  }))
  
output$blow_timer <- renderText({
  blow_timer_react()

})

observeEvent(input$blow_timer, {
  create_timedata_for_blowtimer(15)
})
blow_response <- reactiveValues(response = "Initial")
# observeEvent(input$blow_now, {
#   shinyalert(
#     callbackR = function(x) {
#       blow_response$response <- x
#     },
#     
#     title = "Ready to blow?", text = "Or snooze for X min", type = "input", closeOnEsc = TRUE,
#              closeOnClickOutside = FALSE, html = TRUE, showCancelButton = TRUE,
#              showConfirmButton = TRUE, inputType = "number", inputValue = 15,
#              inputPlaceholder = 15, confirmButtonText = "Snooze",
#              confirmButtonCol = "#AEDEF4", cancelButtonText = "Blow now", timer = 0,
#              animation = TRUE, imageUrl = NULL, imageWidth = 100,
#              imageHeight = 100, className = "",
#              callbackJS = NULL)
# 
# })

observe({
  if (blow_response$response == FALSE) {
    updateTabItems(session, "sidebarmenu", "tab_blow")
    blow_response$response <- "Initial"
    create_timedata_for_blowtimer(180)
  } else if (blow_response$response == "Initial") {
    #do nothing
  } else {
    input_time <- as.numeric(blow_response$response)
    create_timedata_for_blowtimer(input_time)
    blow_response$response <- FALSE
  }
})

blow_timer_react <- reactive({
  invalidateLater(60000 , session)
  blow_data <- luecsv("blow_timer.csv")
  blow_aika <- as.integer(as.ITime(blow_data[, Puhallusaika]))
  blow_pvm <- as.integer(as.IDate(blow_data[, Puhalluspvm])) * 60 * 60 * 24 
  aika <- as.integer(as.ITime(now(tz = "Europe/Helsinki")))
  pvm <- as.integer(as.IDate(now(tz = "Europe/Helsinki"))) * 60 * 60 * 24
  total <- aika + pvm - blow_aika - blow_pvm
  minuutit <- floor(total / 60)
  if(minuutit >= 0 & minuutit < 1) {


    shinyalert(
      callbackR = function(x) {
        blow_response$response <- x
      },
      
      title = "Ready to blow?", text = "Or snooze for X min", type = "input", closeOnEsc = TRUE,
      closeOnClickOutside = FALSE, html = TRUE, showCancelButton = TRUE,
      showConfirmButton = TRUE, inputType = "number", inputValue = 15,
      inputPlaceholder = 15, confirmButtonText = "Snooze",
      confirmButtonCol = "#AEDEF4", cancelButtonText = "Blow now", timer = 0,
      animation = TRUE, imageUrl = NULL, imageWidth = 100,
      imageHeight = 100, className = "",
      callbackJS = NULL)
    
  }
  minuutit
})  

observeEvent(input$loginbutton, {
  shinyalert(
    callbackR = function(x) {
      session$user <- x
    },
    
    title = "Login", text = "Type owner name", type = "input", closeOnEsc = TRUE,
    closeOnClickOutside = FALSE, html = TRUE, showCancelButton = TRUE,
    showConfirmButton = TRUE, inputType = "text",
     confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4", cancelButtonText = "Cancel", timer = 0,
    animation = TRUE, imageUrl = NULL, imageWidth = 100,
    imageHeight = 100, className = "",
    callbackJS = NULL)
  
})
output$Username <- renderText({

  req(session)
  invalidateLater(10000, session)
  session$user
})

#tätä voi käyttää, jos haluaa tallentaa inputtien arvot.
# observeEvent(input$arvo_peli,{
# input_values <<- lapply(reactiveValuesToList(input), unclass)
# saveR_and_send(input_values, "input", "input_values.R")
# })
#load("./external_files/input_values.R")

})
