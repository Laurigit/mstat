
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
slider_martin_mulligan <-  reactiveValues(value = 0, muuttaja = 0) 
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

keymap <- reactiveValues(data  = data.table(Nappain	= c("a", "b"),
                                            set_env = c("normal", "normal"),
                                            env = c("normal", "normal"),
                                            button_id = c("nope", "ei"),
                                            valid_pair = c("e", "b"),
                                            PainoAika = now(),
                                            Painaja = c("Lauri", "Martti"),
                                            uft_nappi = c("a", "b"),
                                            type = "", 
                                            sub_id = ""))
last_simulated_click <- reactiveValues(time = now())
###############

user_logged <- reactiveValues(count = 0)


shinyServer(function(input, output, session) {

  required_data("STAT_VOITTOENNUSTE", saveR = TRUE)
  
  #load_scripts.R
 # print(session$clientData)
observe({
  #seuraa damagea ja turndataa
  depen <- turnData$turn
  dependd <- life_totals$data
  #beep()
})
  inputLoop <- reactiveValues(timeStamp = now(),
                              allow_change = TRUE,
                              which_input_changed = "",
                              input_data = NULL)
  observe({
    req(               slider_martin_mulligan$value, 
                         slider_laurin_virhe$value,
                        slider_martin_virhe$value,
                       slider_laurin_landit$value, 
                   slider_martin_landit$value, 
                      slider_laurin_lifet$value, 
                       slider_martin_lifet$value, 
                        slider_vuoroarvio$value, 
                       slider_laurin_kasikortit$value, 
                         slider_martin_kasikorit$value)
    # vanhat_arvot <- data.table(versio = "vanha",
    #                            slider_laurin_mulligan = 1, 
    #                            slider_martin_mulligan = 2, 
    #                            slider_laurin_virhe =3,
    #                            slider_martin_virhe =4,
    #                            slider_laurin_landit = 5, 
    #                            slider_martin_landit = 6, 
    #                            slider_laurin_lifet = 7, 
    #                            slider_martin_lifet = 8, 
    #                            slider_vuoroarvio =9, 
    #                            slider_laurin_kasikortit = 10, 
    #                            slider_martin_kasikorit =  11)
    
    uudet <- data.table(versio = "uusi", 
                        slider_laurin_mulligan = slider_laurin_mulligan$value, 
                                                     slider_martin_mulligan = slider_martin_mulligan$value, 
                                                     slider_laurin_virhe = slider_laurin_virhe$value,
                                                     slider_martin_virhe = slider_martin_virhe$value,
                                                     slider_laurin_landit = slider_laurin_landit$value, 
                                                     slider_martin_landit = slider_martin_landit$value, 
                                                     slider_laurin_lifet = slider_laurin_lifet$value, 
                                                     slider_martin_lifet = slider_martin_lifet$value, 
                                                     slider_vuoroarvio = slider_vuoroarvio$value, 
                                                     slider_laurin_kasikortit =  slider_laurin_kasikortit$value, 
                                                     slider_martin_kasikorit =  slider_martin_kasikorit$value)
    
    if(is.null(isolate(inputLoop$input_data))) {
      inputLoop$input_data <- uudet[1 != 0]
      inputLoop$input_data [, versio := "vanha"]
    }
    
    yhdiste <- rbind(uudet, isolate(inputLoop$input_data))
    melttaus1 <- suppressWarnings(melt.data.table(yhdiste, id.vars =  c("versio")))
    melttaus1[, diff := var(value), by = variable]
    muuttunut_input <- melttaus1[diff != 0][1, as.character(variable)]
    #jos ei mikaan ei muuttunu, niin ei muuteta UI:ta
  #  print("uus input")
  #  print(muuttunut_input)
  #  print("vanha input")
  #  print(isolate(inputLoop$which_input_changed))
  #  print("session")
  #  print(isolate(session$user))
    erotus <- difftime(now(), isolate(inputLoop$timeStamp))
   
  #  print(erotus)
    isolate(if(is.na(muuttunut_input)){
    #  print("denied, ei muutoksia")
      inputLoop$allow_change <- FALSE
    } else {
      isolate (if ( erotus > 1  | muuttunut_input != inputLoop$which_input_changed) {
   #     print("allow")
        inputLoop$allow_change <- TRUE
        inputLoop$timeStamp <- now()
        uudet[, versio := "vanha"]
        inputLoop$input_data <- uudet
        inputLoop$which_input_changed <- muuttunut_input
        
        
      } else {
  #     print("denied")
        inputLoop$allow_change <- FALSE
      })
    
    })
    
  }, priority = 1000001)
  
  
  

  
  
  
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

if(session$user == "overlay") {
js$hidehead('none')
shinyjs::addClass(selector = "body", class = "sidebar-collapse")
updateTabItems(session,"sidebarmenu", "tab_overlay") 
} 
load_data_from_DB()
  
  sourcelist <- data.table(polku = c(dir("./scripts/", recursive = TRUE)))
  sourcelist[, rivi := seq_len(.N)]
  suppressWarnings(sourcelist[, kansio := strsplit(polku, split = "/")[[1]][1], by = rivi])
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


  output$results = renderPrint({
    intToUtf8(input$mydata[[1]])
  })
 

  # observeEvent(input$mydata, {
  #   ekakirjain <- str_sub(intToUtf8(input$mydata[[1]], 1, 1))
  #   print(ekakirjain)
  #   if (ekakirjain == "x") {
  #     shinyjs::hide(id = "hideBox")
  #   } else {_a
  #     shinyjs::show(id = "hideBox")
  #   }
  # })
  
  local_keymap <- reactiveValues(env = "normal", aika = now(), prev_key = "")
  
  observe({
    print("ENVI NORMAALIKSI")
    take_dep <- turnData$turn
    take_dep <- damage_data$data
    local_keymap$env <- "normal"
    temp <- isolate(keymap$data)
    temp[, Nappain := ""]
    keymap$data <- temp
  })
  
  observeEvent(input$mydata, {
    required_data("ADM_KEY_MAP")
    aakkoPainallus_input <- intToUtf8(input$mydata[[1]])
    isolate(enviro <- local_keymap$env)
    isolate(enviro_aikaEro <- as.numeric(now()) - as.numeric(local_keymap$aika))
    local_keymap$aika <- now()
    print("AIKAERo")
    isolate(print(enviro_aikaEro))
    if (local_keymap$prev_key != aakkoPainallus_input | enviro_aikaEro > 1) {
        
      local_keymap$prev_key <- aakkoPainallus_input
      painaja_uus <- session$user
    #  tempData <-  keymap$data
    #  tempData[Painaja == painaja_uus, ':=' (aakkoPainallus = aakkoPainallus_input,
                                              #  Aika = now())]
    #  tempData <-  keymap$data
     # my_keypress <- tempData[Painaja == session$user, aakkoPainallus]
      toiminnot <- ADM_KEY_MAP[Nappain == aakkoPainallus_input]
      print("ekavaihe")
      print(toiminnot)
      if (nrow(toiminnot) > 0 ){
        
        if (enviro_aikaEro > 20) {
  
          local_keymap$env <- "normal"
          enviro <- "normal"
          print("envi muuttu aikaeron takia normaaliksi")
        }
        
        my_action_row <- toiminnot[env == enviro]
        print("envin jalkeen action row")
        print(my_action_row)
        print("envi oli")
        isolate(print(local_keymap$env))
        if (nrow(my_action_row) > 0) {
          my_action_row[, ':=' (Painaja = painaja_uus,
                   PainoAika = now())]
          
            vihunData <-  keymap$data[Painaja != painaja_uus]
            print(my_action_row)
            print(vihunData)
            uusData <- rbind(my_action_row, vihunData)
            keymap$data <- uusData
        }
      }
    } else {
      warning("painettu sama nappi")
    }
  })
  

  #envs are "normal", shift, deal9+, lose9+
  

  observe({
    req(keymap$data)
  if (session$user %in% c("Lauri", "Martti")) {
    my_action_row <- keymap$data[Painaja == session$user]
    
    #do we need validation
    print("toka vaihe")
    print(my_action_row)

      if (my_action_row[, valid_pair] != "") {
        #we need validation, check opponent input
        my_valid_pair <- my_action_row[, valid_pair]
        opp_button_id <- keymap$data[Painaja != session$user & session$user %in% c("Lauri", "Martti"), button_id]
        Opp_time <- keymap$data[Painaja != session$user & session$user %in% c("Lauri", "Martti"), PainoAika]
        my_time <- my_action_row[, PainoAika]
        aikaErotus <- abs(difftime(my_time, Opp_time))
        warning(paste0("näppäinten ero oli", aikaErotus))
        if (opp_button_id == my_valid_pair & aikaErotus < 1.5) {
         
          accept_input <- TRUE
        } else {
          accept_input <- FALSE
        }
      } else if (keymap$data[which.max(PainoAika), Painaja] != session$user) {
        #we dont need validation, but need to check if I pressed the button
        accept_input <- FALSE
      } else {
        accept_input <- TRUE
      }
    
    
    if (accept_input == TRUE) {
      #click actionutton or something else
      #if no button id, then dont press anything. change environment only if
  
      if (nchar(my_action_row[, button_id]) > 0) {
        if (my_action_row[, type] == "") {
      
        #  print("enabled status")
        #  print(isolate(my_action_row[, button_id]))
          #if button is enabled or we dont monitor it, then click it
          isolate(if (is.null(actButtonStatus[[my_action_row[, button_id]]])) {
            print("Nappi painettu")
       
      
            odotusaika <-  max(as.numeric(last_simulated_click$time + 0 - now()), 0)
            last_simulated_click$time <- now()
            Sys.sleep(odotusaika)

            click(my_action_row[, button_id])
          } else {
            if (actButtonStatus[[my_action_row[, button_id]]] == TRUE) {
              print("Nappi painettu")
              #nappi oli enabled
              odotusaika <-  max(as.numeric(last_simulated_click$time + 0 - now()), 0)
              last_simulated_click$time <- now()
              Sys.sleep(odotusaika)
               click(my_action_row[, button_id])
            }
          })
      } else if (my_action_row[, type] == "RadioGroupButtons") {
         # browser()
          group_id <- my_action_row[, button_id]
          button_name <-  my_action_row[, sub_id]
          curr_value <- isolate(eval(parse(text = paste0("input$", group_id))))
          #check if button is selected
          selected <- button_name %in% curr_value
          if (selected == TRUE) {
            new_value <- curr_value[!curr_value %in% button_name]
          } else {
            new_value <- c(curr_value, button_name)
          }
          updateRadioGroupButtons(session, group_id, selected = new_value)
  
        }
      }
      #set environment
     isolate(if (my_action_row[, set_env] != local_keymap$env & my_action_row[, set_env]  != "") {
       print("UUS ENVI ON")
        local_keymap$env <- my_action_row[, set_env]
        isolate(print(local_keymap$env))
      })
    }
  }
  })

  click_groupButton <- function(session, group_id, button_name) {
    #curr_value <- c("kol", "ys", "kas")
   # button_name <- "ys"

    curr_value <- eval(paste0("input$", group_id))
    curr_value <- eval(input$dmg_settings)
    #check if button is selected
    selected <- button_name %in% curr_value
    if (selected == TRUE) {
      new_value <- curr_value[!curr_value %in% button_name]
    } else {
      new_value <- c(curr_value, button_name)
    }
    updateRadioGroupButtons(session, group_id, selected = new_value)
  }

 output$debug_keymap <- renderDataTable({keymap$data})
 output$debug_local_env <- renderText({local_keymap$env})
  
  required_data("STAT_DMG_TURN_ALL")
  required_data("ADM_TURN_DATA_ALL") 
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
    if (session$user != "overlay") {
    updateTabItems(session, "sidebarmenu", "tab_blow")
    }
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
