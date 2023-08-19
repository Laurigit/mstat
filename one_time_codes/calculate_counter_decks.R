library(adagio)
library(shiny)
library(data.table)
library(stringr)
library(shinydashboard)
library(httr)
library(jsonlite)
library(RMySQL)
library(shinyjs)
library(readtext)
library(clipr)
#library(qdapRegex)
#library(magick)
library(dragulaR)
library(lubridate)
library(readxl)
library(zoo)
library(optiRum)
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(shinyWidgets)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)
library(testthat)
library(Rcpp)
library(ggplot2)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

ssdata <- ADM_PELIT[Omistaja_ID == "L" & !is.na(Voittaja), .(Vasen_voitti = Voittaja, Pakka_ID, Vastustajan_Pakka_ID)]
ssdata[,sum(Vasen_voitti)]


ssdata[,sum(Vasen_voitti)]

ssdata[, avain := "key"]
lpakat <- ssdata[, .N, by = .(Pakka_ID)][order(-N)][1:7, Pakka_ID]
mpakat <- ssdata[, .N, by = .(Vastustajan_Pakka_ID)][order(-N)][1:7, Vastustajan_Pakka_ID]
subset_data <- ssdata[Pakka_ID %in% c(lpakat) & Vastustajan_Pakka_ID %in% c(mpakat)]

features <- data.table(expand.grid(0:2))[, avain := "key"]
features[, comb := seq_len(.N)]
joinaa <- merge.data.table(x = subset_data, y = features, by = "avain", all = TRUE, allow.cartesian = TRUE)


features_vihu <- data.table(expand.grid(0:2))[, avain := "key"]
features_vihu[, comb_vihu := seq_len(.N)]
setnames(features_vihu, c("Var1"), c("Vihu_Var1"))

join_vihu <- merge.data.table(x = joinaa, y = features_vihu, by = "avain", all = TRUE, allow.cartesian = TRUE)

join_vihu[, score_kps := ifelse(Var1 == 0 & Vihu_Var1 == 1, -1,
                            ifelse(Var1 == 0 & Vihu_Var1 == 2, 1,
                                   ifelse(Var1 == 1 & Vihu_Var1 == 0, 1,
                                          ifelse(Var1 == 1 & Vihu_Var1 == 2, -1,
                                                 ifelse(Var1 == 2 & Vihu_Var1 == 0, -1,
                                                        ifelse(Var1 == 2 & Vihu_Var1 == 1, 1, 0))))))]
join_vihu[, .N, by = .(score_kps, Var1, Vihu_Var1)]


join_vihu[, winner_helper := ifelse(Vasen_voitti == 0, -1, 1)]

join_vihu[, score := score_kps * winner_helper]
payoff_data <- join_vihu[, .(score = sum(score)), by = .(Pakka_ID , Vastustajan_Pakka_ID,
                                                         Var1 , Vihu_Var1 )]
combs <- data.table(expand.grid(lpakat, 0:2))[, avain := "key"]
combs_martti <-  data.table(expand.grid(mpakat, 0:2))[, avain := "key"]
combs_tot <- merge.data.table(combs, combs_martti, by = "avain", allow.cartesian = TRUE)
combs_tot[Var1.x == 3]

combinations <- data.table(expand.grid(0:2, 0:2, 0:2, 0:2, 0:2, 0:2, 0:2, 0:2, 0:2, 0:2, 0:2, 0:2, 0:2, 0:2))
setnames(combinations, colnames(combinations), c(lpakat, mpakat))
combinations[, scenario_id := seq_len(.N)]
#rm(combinations)
#gc()
combinations[scenario_id == 1]
melttaa <- melt.data.table(combinations, id.vars = "scenario_id")
melttaa[scenario_id == 1]
melttaa[, omistaja := seq_len(.N) <= (nrow(melttaa) / 2)]
lauri <- melttaa[omistaja == TRUE][, .(scenario_id, Pakka_ID = variable, Var1 = value)]#[order(scenario_id, Pakka_ID )]#[, scenario_id := NULL]
lauri[1:100]
martti <- melttaa[omistaja == FALSE][, .(scenario_id, Pakka_ID  = variable, Var1 = value)]#[order(scenario_id, Vastustajan_Pakka_ID )]
joinaalm <- rbind(lauri, martti)


join_score <- payoff_data[joinaalm, on = .(Pakka_ID, Vastustajan_Pakka_ID, Var1, Vihu_Var1)]
joinaalm[scenario_id == 1]
payoff_data[Pakka_ID == 3]
tot_scen_val <- join_score[, .(scen_score = sum(score)), by = scenario_id]
max_score <- tot_scen_val[, max(scen_score)]
winners <- tot_scen_val[scen_score == max_score]
one_winner <- join_score[scenario_id == winners[, min(scenario_id)]]
join_score[scenario_id == 5]
winner_data <- join_score[scenario_id %in% winners[, scenario_id]]
mode_data <- winner_data[, .(moodi = getmode(Var1)), by = Pakka_ID]
mode_data2 <- winner_data[, .(moodi = getmode(Vihu_Var1)), by = Vastustajan_Pakka_ID]
winner_data[, .N, by = .(Pakka_ID, Var1)][order(Pakka_ID, Var1)]
winner_data[1:100]

winner <- tot_scen_val[order(-scen_score)][1, scenario_id]
winscen <- join_score[scenario_id ==  winner ]
sspakat <- STG_PAKAT[, .(Pakka_ID = as.character(Pakka_ID), Pakka_NM)]
joinname <- sspakat[winscen, on = .( Pakka_ID)]
joinnameM <- sspakat[joinname, on = .( Pakka_ID = Vastustajan_Pakka_ID)]
cleanM <- joinnameM[, .(Pakka_NM, KPS = Vihu_Var1)]
cleanL <- joinnameM[, .(Pakka_NM = i.Pakka_NM, KPS = Var1)]
appendaa <- rbind(cleanM, cleanL)[order(KPS)]
join_vihu[Pakka_ID == 4 & Var1 == 1, sum(winner_helper)]
hist(join_vihu[,  winner_helper])
join_vihu[score == 3]

setDT(join_vihu)[ , ID_oma := .GRP, by = Pakka_ID] 
setDT(join_vihu)[ , ID_vihu := .GRP, by = Vastustajan_Pakka_ID] 
payoff_data <- join_vihu[, .(score = sum(score)), by = .(ID_oma, Vastustajan_Pakka_ID,
                                                         comb, comb_vihu)]


ssdata[, .N, by = .(Pakka_ID)][]
3^20


payoff_data[, .(sum(score)), by = .(Pakka_ID, comb, comb_vihu)]

payoff_func <- function()

hist(payoff_data[,  score])
join_vihu[, sum(winner_helper)]
join_vihu[Pakka_ID == 1 & Var1 == 0 & Var2 == 1 & Vihu_Var1 == 0, sum(winner_helper)]
join_vihu[Pakka_ID == 51 & Vastustajan_Pakka_ID == 59 & Var1 == 0 & Var2 == 1 & Vihu_Var1 == 0]
join_vihu[ , .(sum(score)), by = .(comb, Pakka_ID)][1:100]
payoff_data[Pakka_ID == 50, .(sum(score))]

payoff_data[(Pakka_ID == Vastustajan_Pakka_ID)]

pakat <- sort(as.numeric(unique(c(payoff_data[, Pakka_ID], payoff_data[, Vastustajan_Pakka_ID]))))
oma_pakat <- sort(as.numeric(unique(c(payoff_data[, Pakka_ID]))))
vihu_pakat <- sort(as.numeric(unique(c(payoff_data[, Vastustajan_Pakka_ID]))))

payoff <- function(input_i, input_j, payoff_data) {
  
  input_data <- data.table(input_i, input_j)
  input_data[input_i == 1]
  payoff_data[Pakka_ID  == 1 & Var2 == 0, sum(score)]
  
  # payoff_data[Pakka_ID == 1 & Var1 == 0 & Var2 == 0 & Var3 == 0 & Vihu_Var1   == 1 & Vihu_Var2 == 0 & Vihu_Var3 == 1]
  # 
  # dastaa <- dcast.data.table(input_data, formula = input_i ~ input_j)
  # lefti <- dastaa[input_1]
  # 
  # leftside <- payoff_data[, .(i = Pakka_ID, Var1, Var2, Var3, score)]
  # rightside <- payoff_data[, .(i = Vastustajan_Pakka_ID , Var1 = , Var2 = , Var3 =, score)]
  
  browser()
  input_x
  xxx <- input_y
  return(xxx)
}

res <- MILPModel() %>%
  #  add_variable(y[k], k = 1:20, type = "binary") %>%
  add_variable(x[i], i = oma_pakat,  type = "integer", lb = 1, ub = 8) %>%
  add_variable(y[a], a = vihu_pakat,  type = "integer", lb = 1, ub = 8)  %>%
  
  set_objective(sum_expr(x[i, j] * payoff(i, j, payoff_data), i = pakat, j = 1:3)) %>%
  
  
  solve_model(with_ROI(solver = "symphony", verbosity = -2))
#res %<% solve_model(with_ROI(solver = "glpk", verbose = FALSE))
# objective_value(res)
#print(difftime(Sys.time(), start_time))

# dt_result <- data.table(get_solution(res, x[i, j, k]))[value > 0][order(i)]


raw <- as.data.table(res$solution, keep.rownames = TRUE)[V2 > 0]
res <- nrow(raw)
second_res <- raw[nrow(raw), str_extract(str_sub(V1, 3, -2), "(?<=,)[^,]*(?=,)")]
