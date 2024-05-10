library(tidyverse)

# Load data
motions_polarization <- readRDS("data/motions_polarization.rds")

# #################################################################################
# Prerequisites

# seats per party per legislative period
seat_counts_by_legislative_period <- list(
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
# L/R Distance

calculate_distance_for_involved_parties <- function(involved_parties, legislative_period) {
  if (!legislative_period %in% names(seat_counts_by_legislative_period)) {
    stop("Legislative period not found in seat counts list.")
  }
  
  seats <- seat_counts_by_legislative_period[[legislative_period]]
  involved_parties <- str_split(involved_parties, ",")[[1]] |> str_trim()
  involved_parties <- involved_parties[involved_parties %in% row.names(distance_matrix)]
  
  if (length(involved_parties) == 0) {
    return(NA)
  }
  
  if (length(involved_parties) == 1) {
    # Handling the case where only one party is involved
    party_distances <- distance_matrix[involved_parties, , drop = FALSE]
    total_distance <- sum(party_distances * seats)
    print(paste("Total distance for single involved party", involved_parties, ":", total_distance))
    return(total_distance)
  }
  
  if (length(involved_parties) == length(row.names(distance_matrix))) {
    return(0)  # All parties are involved, hence distance is 0
  }
  
  non_involved_parties <- setdiff(row.names(distance_matrix), involved_parties)
  total_distance <- 0
  
  if (length(non_involved_parties) == 0) {
    return(0)  # All valid parties are involved, hence no distance calculation needed
  }
  
  for (party in non_involved_parties) {
    print(paste("Calculating for non-involved party:", party))
    # Extracting distances and ensuring it is numeric
    distances <- distance_matrix[party, involved_parties]
    print(distances)
    avg_distance <- mean(as.numeric(distances))  # Ensure it's numeric
    print(paste("Average distance for", party, "to involved parties:", avg_distance))
    
    # Weight by the number of seats this party holds
    total_distance <- total_distance + avg_distance * seats[[party]]
  }
  
  return(total_distance)
}

# Example usage
# calculate_distance_for_involved_parties("Mitte, GRÜNE, SP, SVP", "50")  # Expecting different results for different setups
# calculate_distance_for_involved_parties("Mitte, FDP, SP", "50")
# calculate_distance_for_involved_parties("SVP", "50")

# create column
motions_polarization <- motions_polarization %>%
  rowwise() %>%  # Apply the function to each row individually
  mutate(lr_distance_total = calculate_distance_for_involved_parties(
    involved_parties, as.character(SubmissionLegislativePeriod)
  ))


# min max normalization

# Calculate min and max values for the entire 'lr_distance_total' column first
min_distance <- min(motions_polarization$lr_distance_total, na.rm = TRUE)
max_distance <- max(motions_polarization$lr_distance_total, na.rm = TRUE)


# Adding 'lr_distance_normalized' column using min-max normalization
motions_polarization <- motions_polarization %>%
  mutate(
    lr_distance_normalized = (lr_distance_total - min_distance) / (max_distance - min_distance)
  )

# #################################################################################
# saving
saveRDS(motions_polarization, "data/motions_polarization_with_lr.rds")


