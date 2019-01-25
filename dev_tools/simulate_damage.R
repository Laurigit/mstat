required_data("ADM_CURRENT_DMG")
required_data("ADM_TURN_SEQ")

library(tidyverse)
library(ggplot2)
library(reshape2)
library(lubridate)
library(grid)
library(gridExtra)



TSID <- sort(round(runif(25, 1, 40), 0))
Amount_Amount <-(round(3 / rbeta(25, 3.5, 1.5)) - 4)
Amount_Amount <- pmax(1, Amount_Amount)
Lifegain <- ifelse(runif(25, 0, 20) > 16, -1, 1)
Amount <- Amount_Amount * Lifegain
DID_L <- seq(1:25) * 2
DID_M <- seq(1:25) * 2 -1
Target_player <- ifelse(runif(25, 0, 20) > 9, "Martti", "Lauri")
reverse_source <- ifelse(runif(25, 0, 20) > 18, -1, 1)
Combat_damage <- ifelse(runif(25, 0, 20) > 5, 1, 0)
Dmg_source <- data.table(Target_player, reverse_source)
Dmg_source[, Dmg_source := ifelse(Target_player == "Lauri", ifelse(reverse_source == -1, "Lauri", "Martti"), "Lauri")]
Dmg_source[, Peli_ID := 1030]
dtL <- data.table(DID = DID_L, Input_Omistaja_NM = "Lauri")
dtM <- data.table(DID = DID_M, Input_Omistaja_NM = "Martti")

curr_data <- cbind(Amount, Target_player, Dmg_source, Combat_damage, TSID)
dtLCurr<- cbind(dtL, curr_data)
dtMCurr<- cbind(dtM, curr_data)

dtLCurr_Turn <- dtLCurr[ADM_TURN_SEQ, on = "TSID"][, Target_player := ifelse(is.na(Target_player), "Lauri", Target_player)]
dtMCurr_Turn <- dtMCurr[ADM_TURN_SEQ, on = "TSID"][, Target_player := ifelse(is.na(Target_player), "Martti", Target_player)]
total_all <- rbind(dtLCurr_Turn, dtMCurr_Turn)[order(TSID, DID)]
total <- total_all[, .(DID,
                       Amount = ifelse(is.na(Amount), 0, Amount),
                       Target_player,
                       Dmg_source,
                       Combat_damage,
                       Input_Omistaja_NM,
                       TSID,
                       Peli_ID,
                       End_phase,
                       Starters_turn,
                       Turn)]
graphInput <- total[,. (DID,
                      Amount,
                      Target_player,
                      Dmg_source,
                      Combat_damage,
                      Input_Omistaja_NM,
                      TSID,
                      Peli_ID,
                      End_phase,
                      Starters_turn,
                      Turn)]
Aloittaja <- "Lauri"
starting_life <-  20

# tse <- ADM_TURN_SEQ
# join_tse <- graphInput[ADM_TURN_SEQ, on = "TSID"]
# join_tse[,':=' (Amount = ifelse(is.na(Amount), 0, Amount))]
#only for simul
join_tse <- graphInput
  join_tse[, Combat_damage := ifelse(End_phase == TRUE, 0, Combat_damage)]
####
  join_tse[, Target_Player_Turn := (Target_player == Aloittaja) == Starters_turn]
aggr_dmg <- join_tse[, .(Amount = sum(Amount)
                         ),
                       by = .(
                              
                              Target_player,
                             # Dmg_source,
                             # Combat_damage,
                              
                              Turn,
                              Target_Player_Turn,Starters_turn)]
aggr_dmg[, lifegain := ifelse(Amount < 0, TRUE, FALSE)]
aggr_dmg[, half_turn := (Turn  + ifelse(Starters_turn == TRUE, 0, 0.5))]
aggr_dmg[, cum_dmg := cumsum(Amount), by = Target_player]
aggr_dmg[, cum_lifetotl := starting_life - cum_dmg]
aggr_dmg[, cum_life_start_of_turn := cum_lifetotl + Amount]
#aggr_dmg <- aggr_dmg[order(TSID)]



set.seed(NULL)
data <- data.frame(date = seq(1, 372, by = 31) %>% as_date)
data <- data %>% 
  mutate(new = abs(rnorm(12, 100, 10)) %>% round(0)) %>% 
  mutate(churn = abs(rnorm(12, 50, 30)) %>% round(0)) %>% 
  mutate(net = new - churn)  %>% 
  mutate(eop = cumsum(net)) %>% 
  select(-net)

data

# Define the width of segment
step <- 0.4*(max(data$date) - min(data$date))/(nrow(data) - 1)

# Define ymax of segment
data <- data %>% 
  mutate(ymax = eop + churn)

aggr_dmg[, ':=' (ymax = cum_life_start_of_turn,
                 xmin = half_turn,
                 xmax = half_turn + 0.5,
                 ymin = cum_lifetotl,
                 value = Amount)]
aggr_dmg<-aggr_dmg[Target_player == "Martti"]

last_dmg <- aggr_dmg[Amount > 0, max(half_turn)]

aggr_dmg_cut <- aggr_dmg[half_turn <= last_dmg]
# Define ymin of segment
# df <- data %>% 
#   melt(id.vars = c("date", "eop", "ymax")) %>% 
#   mutate(ymin = ymax - value) %>% 
#   rename(group = variable)
# 
# # Define xmin and xmax of segments
# df <- df %>% 
#   mutate(xmin = case_when(
#     group == "new" ~ date - step,
#     TRUE ~ date 
#   )) %>% 
#   mutate(xmax = case_when(
#     group == "new" ~ date,
#     TRUE ~ date + step
#   ))

# Create waterfall chart
plot_ymax <- aggr_dmg_cut[, max(ymax)]
plot_xmax <- aggr_dmg_cut[, max(half_turn)]

aggr_dmg_cut %>% 
  arrange(half_turn) %>% 
  ggplot() +
  geom_rect(aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax,
                fill =  factor(lifegain))) + scale_fill_manual(values = c("red", "green4")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,plot_ymax), breaks = seq(0, plot_ymax, by = 5)) +   
  scale_x_continuous(expand = c(0, 0), limits = c(1, plot_xmax) ,breaks = seq(1:plot_xmax)) + 
# ylim(min = 0, max = 20) +
   # geom_segment(aes(x = xArrow, y = ymax, xend = xArrow, yend = ymin),
   #               arrow = arrow(length = unit(0.5, "cm")), size = 1) +
  geom_line(aes(half_turn, (cum_life_start_of_turn)), col = "dodgerblue4", size = 1)   -> p1
p1 
#p2 <- p1 +  expand_limits(x=c(1,10), y=c(0,20))
p3 <- p1 + 

p3
# p4 <- p3 +  theme(axis.title.x = element_text(family="Times",size=20,
#                                     face="bold",colour = "Black",vjust=-1,hjust=0.5))+
#   theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
p4
# 
# + scale_fill_gradient2(midpoint = 0,
#                        low = "green4",
#                        high = "red",
#                        mid = "blue",
#                        limits = c(-1, 1))

# Create waterfall chart

# Create data for line chart
# df2 <- df %>% select(date, eop) %>% distinct()
# 
# # Optimize colors, themes & add lines
# p2 <- p1  + 
#   geom_line(aes(date, eop), col = "dodgerblue4", size = 1) +
#   geom_point(aes(date, eop), col = "dodgerblue4", size = 2.5) +
#   geom_text(aes(date, eop, label = eop), vjust = 1.2, 
#             hjust = -0.1) +
#   scale_fill_manual(values = c("grey60", "coral2")) +
#   theme_minimal() +
#   theme(
#     axis.line = element_line(color = "gray40", size = 0.5),
#     legend.position = "top") +
#   scale_x_date(breaks = data$date,
#                date_labels = "%b") +
#   theme(panel.grid.minor.x = element_blank(),
#         legend.title = element_blank()) +
#   ggtitle("Overview of active users") +
#   xlab("Date") + 
#   ylab("Number of active users")
# p2
