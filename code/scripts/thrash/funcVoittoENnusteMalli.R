# peliData <- luecsv("pelit.csv")
# LP<-5
# MP<-5
# LMull <- 0
# MMull <- 0
# LHinta<-100
# MHinta <-100
# LKortit <- 50
# MKortit  <- 50
# Aloittaja <- 0
# maxTurausInclude <- 14
# peliData <- luecsv("pelit.csv")
# pakat<-omaReadJson("./external_files/")
# pfi_data<-pakkaUutuusProsentti(pakat)
# peliData_ja_pfi <-  funcLiitaPelit_ja_Pysyvyys(pfi_data, peliData)
# tulos <- voittoEnnusteMallit(peliData_ja_pfi)
voittoEnnusteMallit <- function(maxTurausInclude = 1000000000) {
  required_data("ADM_PELIT")
  peliData_ja_pfi<- ADM_PELIT
  pelidata_joined_pakkatiedot <-  peliData_ja_pfi[!is.na(Voittaja) & Turnaus_NO <= maxTurausInclude]

  #loopataan kaikki pelit
#  Laurin_pakat <- pelidata_joined_pakkatiedot[Turnaus_NO <= maxTurausInclude,.N, by = ,.(Pakka_ID)][,N:=NULL][, avain := 1]
 # Martin_pakat <-  pelidata_joined_pakkatiedot[Turnaus_NO <= maxTurausInclude,.N, by = ,.(Martin_pakka)][,N:=NULL][, avain := 1]
  pakkayhdistelmat <-pelidata_joined_pakkatiedot[,.N, by = ,.(Pakka_ID, Vastustajan_Pakka_ID)][,N:=NULL]
    #luo muuttujat. Kertoimet sen takia, että otetaan yhteiskerroin vaan VS_peliin.
  #looppaa_kombot
  #ota vain yhdistelmat
  
  #korjaa data vanhaan muotoon
  laurin_pelit <- pelidata_joined_pakkatiedot[Omistaja_ID == "L", .(Laurin_pakka = Pakka_ID,

                                                                  
                                                                  Voittaja,
                                                                  Laurin_mulligan = Mulligan,
                                                                  Laurin_pysyvyys_pct = Pakka_form_pct,
                                                                  Aloittaja,
                                                                  hinta_lauri = Hinta,
                                                                  laurin_kortti_lkm = Kortti_lkm_manastack, Peli_ID)]
  #haetaan siihen martin data
  
  martin_pelit <- pelidata_joined_pakkatiedot[Omistaja_ID == "M",
                                                     .(Martin_mulligan = Mulligan,
                                                       Martin_pysyvyys_pct = Pakka_form_pct,
                                                       hinta_martti = Hinta,
                                                       martin_kortti_lkm = Kortti_lkm_manastack,
                                                       Martin_pakka = Pakka_ID,
                                                      
                                                       Peli_ID)]
  #joinaa
  joinpelit <- laurin_pelit[martin_pelit, on = "Peli_ID"]
  
  
  Laurin_pakat <- joinpelit[,.N, by = ,.(Laurin_pakka)][, N := NULL][, avain := 1]

  
  Martin_pakat <-  joinpelit[,.N, by = ,.(Martin_pakka)][,N := NULL][, avain := 1]

  
  pakkayhdistelmat <-merge(Laurin_pakat, Martin_pakat, all = TRUE, by = "avain", allow.cartesian = TRUE)
  pakkayhdistelmat[, avain := NULL]
  
  for(kierros in 1:nrow(pakkayhdistelmat)) {
  
  #filtteröi data
    pakat <- pakkayhdistelmat[kierros]
    LP <- pakat[, Laurin_pakka]
    MP <- pakat[, Martin_pakka]
    
    
    # LP <-3
    # MP <- 11
    
  subset_pelit <- joinpelit[(Laurin_pakka == LP | Martin_pakka == MP)]
  
  subset_pelit[, ':=' (Mull_diff =  Martin_mulligan - Laurin_mulligan,
                       VS_peli_bool = ifelse(Laurin_pakka == LP & Martin_pakka == MP, 1, 0),
                       # L_kerroin = ifelse(Laurin_pakka == LP, Laurin_pysyvyys_pct,1),
                       # M_kerroin = ifelse(Martin_pakka == MP, Martin_pysyvyys_pct, 1),
                       L_kerroin = Laurin_pysyvyys_pct,
                       M_kerroin = Martin_pysyvyys_pct,
                       laurin_keskihinta = hinta_lauri / laurin_kortti_lkm,
                       martin_keskihinta = hinta_martti / martin_kortti_lkm)]
  
  
  
  subset_pelit[, ':=' (weight = L_kerroin * M_kerroin,
                       keskiHintaEro = log((laurin_keskihinta / martin_keskihinta), base = exp(1)))]
  
  
  #analyysisarakkeet
  analyse_cols <- subset_pelit[, .(Laurin_pakka,
                                   Martin_pakka,
                                   Voittaja,
                                   Mull_diff,
                                   VS_peli_bool,
                                   weight,
                                   Aloittaja,
                                   keskiHintaEro)]
  count_vs_peli <- analyse_cols[, sum(VS_peli_bool)]
  if (count_vs_peli > 6) {
  model <- suppressWarnings(glm(data = analyse_cols, 
               formula = Voittaja ~ Aloittaja + Mull_diff  + VS_peli_bool + keskiHintaEro, 
               family = binomial(link = "logit")
               ,weights = weight))
  } else if (count_vs_peli > 0) {
    model <- suppressWarnings(glm(data = analyse_cols, 
                 formula = Voittaja ~ Aloittaja + Mull_diff  + keskiHintaEro, 
                 family = binomial(link = "logit")
                 ,weights = weight))
  } else {
    dummydata <- data.table(Voittaja = c(1, 0, 1, 0), Aloittaja = c(1, 1, 0, 0))
    model <- suppressWarnings(glm(data = dummydata, 
                                  formula = Voittaja ~ Aloittaja,
                                  family = binomial(link = "logit")
                                  ))
  } 

  
  model_list <- list()
  model_list[[1]]<-model
  pakkayhdistelmat[kierros, malli:=.(model_list)]
}

  #predict
  
 # aloittaja_for_dt <- Aloittaja
  
#  pred_data <- data.table(Mull_diff = 0 - 0, VS_peli_bool = 1, Aloittaja = 0,
                         # keskiHintaEro = log((1 / 1)/(1 / 1), base = exp(1)))
  #print(pred_data)
 # prediction <- predict(pakkayhdistelmat[1,model], pred_data, type = "response")
  return(pakkayhdistelmat)
  #analyse_cols[,fix_voit := ifelse(Voittaja==0, -1, 1)]
  #analyse_cols[, weight_fix := weight*fix_voit]
  #analyse_cols[, ':=' (cum_Voit = cumsum(fix_voit), cum_Voit_weight = cumsum(weight_fix))]
  #analyse_cols[, tulos := (predict(model, analyse_cols, type = "response"))]
  #analyse_cols[,.N,by=.(tulos, Mull_diff, Aloittaja, VS_peli_bool)]
}
