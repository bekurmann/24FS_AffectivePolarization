# Load required packages
library(tidyverse)
library(wnominate)
library(pscl)

# Load the data
votings <- readRDS("data/votings.rds")

# Ensure proper format and filter for votes within 2023
votings <- votings %>%
  mutate(VoteEndWithTimezone = as.POSIXct(VoteEndWithTimezone, format = "%Y-%m-%d %H:%M:%S")) %>%
  filter(year(VoteEndWithTimezone) == 2023) %>%
  mutate(mp_id = paste("mp", FirstName, LastName, PersonNumber, sep = "_"))

# Select the most recent vote per parliamentarian per motion
votings_transformed <- votings %>%
  arrange(IdVote, mp_id, VoteEndWithTimezone) %>%
  group_by(IdVote, mp_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(DecisionCode = case_when(
    Decision == 1 ~ 1,  # Ja
    Decision == 2 ~ 0,  # Nein
    TRUE ~ NA_real_    # NA for all other cases (did not vote or absent)
  ))

# Pivot data to wider format
pivot_data <- votings_transformed %>%
  select(mp_id, IdVote, DecisionCode) %>%
  pivot_wider(
    names_from = IdVote,
    values_from = DecisionCode,
    values_fill = list(DecisionCode = NA_real_),
    id_cols = mp_id
  )

# Convert to matrix for W-NOMINATE
votings_matrix <- as.matrix(pivot_data[, -1])  # Exclude the mp_id column for the matrix
row.names(votings_matrix) <- pivot_data$mp_id  # Use mp_id as row names

# Verify matrix dimensions
print(dim(votings_matrix))

# Create the rollcall object correctly
rc_object <- rollcall(votings_matrix, yea = 1, nay = 0, missing = NA,
                      legis.names = row.names(votings_matrix),
                      vote.names = colnames(votings_matrix))

# Set polarity using the index of the most leftist MP
polarity_index <- which(row.names(votings_matrix) == "mp_Balthasar_Glättli_4093")

# Run W-NOMINATE with specified polarity
nominate_result <- wnominate(rc_object, dims = 1, polarity = polarity_index)

# Summary and plots
print(summary(nominate_result))
plot(nominate_result)
plot.coords(nominate_result)

# Create a data frame of legislator positions
legislator_positions <- data.frame(
  Names = row.names(votings_matrix),
  Coordinates = nominate_result$legislators$coord1D
)

# Check legislator positions
head(legislator_positions)
min(legislator_positions)

# save
saveRDS(legislator_positions, "data/legislator_positions.rds")

# open (for quick access)
legislator_positions <- readRDS("data/legislator_positions.rds")


