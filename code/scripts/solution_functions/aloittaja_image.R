aloittaja_image <- function(Aloittaja_input, Divari) {
  Divarit <- data.table(Divarit = c(1, 2, 3, 4))
  Kortit <- c("Avacyn, Angel of Hope",
              "Hellkite Charger",
              "Blizzard Specter",
              "Ankle Shanker",
              "Elesh Norn, Grand Cenobite",
              "Nekrataal",
              "Sanctified charge",
              "Knight of meadowgrain")
  Aloittaja <- data.table(Aloittaja = c(1, 0))
  crossi <- CJ_dt(Divarit, Aloittaja)
  pindgi <- cbind(crossi, Kortit)
  tulos <- pindgi[Divarit == Divari & Aloittaja == Aloittaja_input, Kortit]
  if (length(tulos) == 0) {
    tulos <- "Totally Lost"
  }
  return(tulos)
}
