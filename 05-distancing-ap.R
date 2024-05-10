library(tidyverse)

motions <- readRDS("data/motions_polarization_with_lr.rds")

# #################################################################################
# Prerequisites

# seats per party per legislative period
seat_counts_by_legislative_period <- list(
  "50" = c(GRÜNE = 11, SP = 55, GLP = 7, Mitte = 34, FDP = 33, SVP = 65),
  "51" = c(GRÜNE = 28, SP = 53, GLP = 16, Mitte = 28, FDP = 29, SVP = 53)
)

# Function to retrieve seat counts for a given legislative period per party
get_seat_count <- function(data, legislative_period, party) {
  if (!legislative_period %in% names(data)) {
    stop("Invalid legislative period. Please choose a valid one.")
  }
  
  if (!party %in% names(data[[legislative_period]])) {
    stop("Invalid party name. Please choose a valid one.")
  }
  
  data[[legislative_period]][[party]]
}

# Example usage:
# get_seat_count(seat_counts_by_legislative_period, "50", "SP")

# #################################################################################
# ap matrix

like_dislike_matrix <- data.frame(
  GRÜNE = c(mean(c(87, 94, 89)), mean(c(73, 80, 72)), mean(c(66, 59)), mean(c(44.9, 50, 36)), mean(c(32, 38, 30)), mean(c(27, 28, 17))),
  SP = c(mean(c(77, 82, 76)), mean(c(81, 92, 89)), mean(c(66, 59)), mean(c(44.9, 50, 36)), mean(c(32, 38, 30)), mean(c(27, 28, 17))),
  GLP = c(mean(c(64, 59)), mean(c(62, 52)), mean(c(92, 86)), mean(c(62, 48)), mean(c(58, 49)), mean(c(38, 27))),
  Mitte = c(mean(c(44, 56, 40)), mean(c(48, 58, 45)), mean(c(70, 51)), mean(c(80.5, 92, 85)), mean(c(59, 70, 54)), mean(c(48, 56, 42))),
  FDP = c(mean(c(36, 41, 30)), mean(c(39, 46, 35)), mean(c(58, 53)), mean(c(56.7, 64, 55)), mean(c(78.5, 92, 84)), mean(c(59, 64, 53))),
  SVP = c(mean(c(11, 28, 6)), mean(c(12, 30, 8)), mean(c(34, 15)), mean(c(23.6, 46, 35)), mean(c(35, 58, 38)), mean(c(84, 92, 88))),
  row.names = c("GRÜNE", "SP", "GLP", "Mitte", "FDP", "SVP")
)

# #################################################################################
# ap simple

# #################################################################################
# ap extended