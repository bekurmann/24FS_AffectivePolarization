library(tidyverse)

# Load data
motions_polarization <- readRDS("data/motions_polarization.rds")

# #################################################################################
# Prerequisites

# seats per party per legislative period
seat_counts_by_year <- list(
  "50" = c(GRÜNE = 11, SP = 55, GLP = 7, Mitte = 34, FDP = 33, SVP = 65),
  "51" = c(GRÜNE = 28, SP = 53, GLP = 16, Mitte = 28, FDP = 29, SVP = 53)
)

distance_matrix <- data.frame(
  GRÜNE = c(0, 4, 46, 59, 75, 87),
  SP = c(4, 0, 50, 63, 79, 91),
  GLP = c(46, 50, 0, 13, 29, 41),
  Mitte = c(59, 63, 13, 0, 16, 28),
  FDP = c(75, 79, 29, 16, 0, 22),
  SVP = c(87, 91, 41, 28, 22, 0),
  row.names = c("GRÜNE", "SP", "GLP", "Mitte", "FDP", "SVP")
)

get_seat_counts <- function(legislative_period) {
  if (as.character(legislative_period) %in% names(seat_counts_by_year)) {
    return(seat_counts_by_year[[as.character(legislative_period)]])
  } else {
    # Default seat counts or handle the absence of data for a legislative period
    return(c(GRÜNE = NA, SP = NA, GLP = NA, Mitte = NA, FDP = NA, SVP = NA))
  }
}

# #################################################################################
# L/R Distance

# Single Party Distance Calculation
calculate_single_party_distance <- function(party, distance_matrix, seat_counts) {
  distances <- distance_matrix[, party]
  total_distance <- sum(distances * seat_counts)
  avg_distance <- total_distance / sum(seat_counts)
  return(c(total_distance, avg_distance))
}

# Multi-Party Distance Calculation
calculate_multi_party_distance <- function(parties, distance_matrix, seat_counts) {
  distances <- sapply(parties, function(p) distance_matrix[, p])
  mean_distances <- apply(distances, 1, mean)
  total_distance <- sum(mean_distances * seat_counts)
  avg_distance <- total_distance / sum(seat_counts)
  return(c(total_distance, avg_distance))
}

# calculate and add column
motions_polarization <- motions_polarization %>%
  rowwise() %>%
  mutate(
    # Direct computation of total and average distances without intermediate storage
    lr_distance_total = {
      seats_this_period <- get_seat_counts(SubmissionLegislativePeriod)
      parties_vec <- str_split(involved_parties, ",\\s*")[[1]]  # Split involved parties
      
      # Compute distances based on the number of involved parties
      distances <- if (length(parties_vec) == 1) {
        calculate_single_party_distance(parties_vec, distance_matrix, seats_this_period)
      } else {
        calculate_multi_party_distance(parties_vec, distance_matrix, seats_this_period)
      }
      distances[1]  # Total distance
    },
    lr_distance_avg = {
      seats_this_period <- get_seat_counts(SubmissionLegislativePeriod)
      parties_vec <- str_split(involved_parties, ",\\s*")[[1]]  # Split involved parties
      
      # Compute distances based on the number of involved parties
      distances <- if (length(parties_vec) == 1) {
        calculate_single_party_distance(parties_vec, distance_matrix, seats_this_period)
      } else {
        calculate_multi_party_distance(parties_vec, distance_matrix, seats_this_period)
      }
      distances[2]  # Average distance
    }
  ) %>%
  ungroup()


# # #################################################################################
# # testing
# 
# lm1 <- lm(motions_polarization$yes_percent ~ motions_polarization$NormalizedOutsideDistance)
# summary(lm1)
# 
# motions_polarization %>%
#   ggplot(aes(x = TotalOutsideDistance, y = yes_percent, color = passed)) +  
#   geom_point(alpha = 0.7, size = 3) +  
#   geom_smooth(method = "lm", se = TRUE,   
#               aes(group = 1),           
#               color = "black") +  
#   theme_minimal() + 
#   labs(
#     x = "Normalized Outside LR Distance",
#     y = "Percentage of 'Yes'",
#     title = "Scatter Plot of Normalized LR Distance vs. Yes Percentage",
#     color = "failed/passed"  
#   ) 
# 
# lm2 <- lm(motions_polarization$yes_percent ~ motions_polarization$NormalizedAPScore)
# summary(lm2)
# 
# motions_polarization %>%
#   ggplot(aes(x = NormalizedAPScore, y = yes_percent, color = passed)) +  
#   geom_point(alpha = 0.7, size = 3) +  
#   geom_smooth(method = "lm", se = TRUE,   
#               aes(group = 1),             
#               color = "black") +  
#   theme_minimal() + 
#   labs(
#     x = "AP Score",
#     y = "Percentage of 'Yes'",
#     title = "Scatter Plot of Normalized AP Score vs. Yes Percentage",
#     color = "failed/passed"  
#   ) 
# 
# # combine
# 
# lm3 <- lm(motions_polarization$yes_percent ~ motions_polarization$NormalizedOutsideDistance + motions_polarization$NormalizedAPScore)
# summary(lm3)
# 
# motions_polarization %>%
#   ggplot(aes(x = NormalizedAPScore, y = NormalizedOutsideDistance, color = passed)) +  
#   geom_point(alpha = 0.7, size = 3) +  
#   geom_smooth(method = "lm", se = TRUE, 
#               aes(group = 1),             
#               color = "black") +  
#   theme_minimal() +  
#   labs(
#     x = "Normalized AP Distance",
#     y = "Normalized Outside LR Distance",
#     title = "Scatter Plot of Normalized AP Distance vs. Normalized Outside LR Distance",
#     color = "failed/passed"  
#   ) 
# 
