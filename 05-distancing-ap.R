library(tidyverse)

# Load the existing data
motions_polarization <- readRDS("data/motions_polarization_with_lr.rds")

# Prerequisites
seat_counts_by_legislative_period <- list(
  "50" = c(GRÜNE = 11, SP = 55, GLP = 7, Mitte = 34, FDP = 33, SVP = 65),
  "51" = c(GRÜNE = 28, SP = 53, GLP = 16, Mitte = 28, FDP = 29, SVP = 53)
)

# ######################################################################################
# Affective Polarization Matrix
like_dislike_matrix <- data.frame(
  GRÜNE = c(mean(c(87, 94, 89)), mean(c(73, 80, 72)), mean(c(66, 59)), mean(c(44.9, 50, 36)), mean(c(32, 38, 30)), mean(c(27, 28, 17))), 
  SP = c(mean(c(77, 82, 76)), mean(c(81, 92, 89)), mean(c(64, 56)), mean(c(45.3, 52, 39)), mean(c(33, 44, 32)), mean(c(31, 34, 22))),
  GLP = c(mean(c(64, 59)), mean(c(62, 52)), mean(c(92, 86)), mean(c(62, 48)), mean(c(58, 49)), mean(c(38, 27))),
  Mitte = c(mean(c(44, 56, 40)), mean(c(48, 58, 45)), mean(c(70, 51)), mean(c(80.5, 92, 85)), mean(c(59, 70, 54)), mean(c(48, 56, 42))), 
  FDP = c(mean(c(36, 41, 30)), mean(c(39, 46, 35)), mean(c(58, 53)), mean(c(56.7, 64, 55)), mean(c(78.5, 92, 84)), mean(c(59, 64, 53))),
  SVP = c(mean(c(11, 28, 6)), mean(c(12, 30, 8)), mean(c(34, 15)), mean(c(23.6, 46, 35)), mean(c(35, 58, 38)), mean(c(84, 92, 88))),
  row.names = c("GRÜNE", "SP", "GLP", "Mitte", "FDP", "SVP")
)

like_dislike_matrix2 <- data.frame(
  GRÜNE = c(mean(c(87, 94, 89)), mean(c(73, 80, 72)), mean(c(59)), mean(c(44.9, 50, 36)), mean(c(32, 38, 30)), mean(c(27, 28, 17))), 
  SP = c(mean(c(77, 82, 76)), mean(c(81, 92, 89)), mean(c(56)), mean(c(45.3, 52, 39)), mean(c(33, 44, 32)), mean(c(31, 34, 22))),
  GLP = c(mean(c(59)), mean(c(52)), mean(c(86)), mean(c(48)), mean(c(49)), mean(c(27))),
  Mitte = c(mean(c(44, 56, 40)), mean(c(48, 58, 45)), mean(c(51)), mean(c(80.5, 92, 85)), mean(c(59, 70, 54)), mean(c(48, 56, 42))), 
  FDP = c(mean(c(36, 41, 30)), mean(c(39, 46, 35)), mean(c(53)), mean(c(56.7, 64, 55)), mean(c(78.5, 92, 84)), mean(c(59, 64, 53))),
  SVP = c(mean(c(11, 28, 6)), mean(c(12, 30, 8)), mean(c(15)), mean(c(23.6, 46, 35)), mean(c(35, 58, 38)), mean(c(84, 92, 88))),
  row.names = c("GRÜNE", "SP", "GLP", "Mitte", "FDP", "SVP")
)

# ######################################################################################
# Function to calculate affective polarization (1 with sotomo)
calculate_affective_polarization <- function(involved_parties, legislative_period) {
  if (!legislative_period %in% names(seat_counts_by_legislative_period)) {
    stop("Legislative period not found in seat counts list.")
  }
  
  seats <- seat_counts_by_legislative_period[[legislative_period]]
  involved_parties <- str_split(involved_parties, ",")[[1]] |> str_trim()
  involved_parties <- involved_parties[involved_parties %in% row.names(like_dislike_matrix)]
  
  if (length(involved_parties) == 0) {
    return(NA)
  }
  
  total_score <- 0
  all_parties <- row.names(like_dislike_matrix)
  
  # Calculate score for involved parties
  for (party in involved_parties) {
    score_for_self <- like_dislike_matrix[party, party]
    calculated_score <- seats[[party]] * score_for_self
    total_score <- total_score + calculated_score
  }
  
  # Calculate score for non-involved parties
  non_involved_parties <- setdiff(all_parties, involved_parties)
  for (party in non_involved_parties) {
    scores <- like_dislike_matrix[party, involved_parties]
    avg_score <- mean(as.numeric(unlist(scores)), na.rm = TRUE)
    
    if (is.na(avg_score)) {
      avg_score <- 0
    }
    calculated_score = seats[[party]] * avg_score
    total_score <- total_score + calculated_score
  }
  
  return(total_score)
}

# ######################################################################################
# Function to calculate affective polarization (1 without sotomo)
calculate_affective_polarization2 <- function(involved_parties, legislative_period) {
  if (!legislative_period %in% names(seat_counts_by_legislative_period)) {
    stop("Legislative period not found in seat counts list.")
  }
  
  seats <- seat_counts_by_legislative_period[[legislative_period]]
  involved_parties <- str_split(involved_parties, ",")[[1]] |> str_trim()
  involved_parties <- involved_parties[involved_parties %in% row.names(like_dislike_matrix2)]
  
  if (length(involved_parties) == 0) {
    return(NA)
  }
  
  total_score <- 0
  all_parties <- row.names(like_dislike_matrix2)
  
  # Calculate score for involved parties
  for (party in involved_parties) {
    score_for_self <- like_dislike_matrix2[party, party]
    calculated_score <- seats[[party]] * score_for_self
    total_score <- total_score + calculated_score
  }
  
  # Calculate score for non-involved parties
  non_involved_parties <- setdiff(all_parties, involved_parties)
  for (party in non_involved_parties) {
    scores <- like_dislike_matrix2[party, involved_parties]
    avg_score <- mean(as.numeric(unlist(scores)), na.rm = TRUE)
    
    if (is.na(avg_score)) {
      avg_score <- 0
    }
    calculated_score = seats[[party]] * avg_score
    total_score <- total_score + calculated_score
  }
  
  return(total_score)
}

# ######################################################################################
# Adding the new column(s) with the original AP score
motions_polarization <- motions_polarization %>%
  rowwise() %>%
  mutate(ap_score_total = calculate_affective_polarization(involved_parties, as.character(SubmissionLegislativePeriod)))

# Inverting the AP score to make higher scores indicate higher polarization
max_score <- max(motions_polarization$ap_score_total, na.rm = TRUE)
motions_polarization <- motions_polarization %>%
  mutate(ap_score_total_inverted = max_score - ap_score_total)

# Min-max normalization on the inverted scores
min_score <- min(motions_polarization$ap_score_total_inverted, na.rm = TRUE)
max_score <- max(motions_polarization$ap_score_total_inverted, na.rm = TRUE)

motions_polarization <- motions_polarization %>%
  mutate(
    ap_score_normalized = (ap_score_total_inverted - min_score) / (max_score - min_score)
  )

# #########################
motions_polarization <- motions_polarization %>%
  rowwise() %>%
  mutate(ap_score_total2 = calculate_affective_polarization2(involved_parties, as.character(SubmissionLegislativePeriod)))

# Inverting the AP score to make higher scores indicate higher polarization
max_score2 <- max(motions_polarization$ap_score_total2, na.rm = TRUE)
motions_polarization <- motions_polarization %>%
  mutate(ap_score_total_inverted2 = max_score2 - ap_score_total2)

# Min-max normalization on the inverted scores
min_score2 <- min(motions_polarization$ap_score_total_inverted2, na.rm = TRUE)
max_score2 <- max(motions_polarization$ap_score_total_inverted2, na.rm = TRUE)

motions_polarization <- motions_polarization %>%
  mutate(
    ap_score_normalized2 = (ap_score_total_inverted2 - min_score2) / (max_score2 - min_score2)
  )

# ######################################################################################

# Save the updated dataframe
saveRDS(motions_polarization, "data/motions_polarization_with_lr_ap_inverted.rds")

# Print the updated dataframe to check the new columns
print(motions_polarization)
