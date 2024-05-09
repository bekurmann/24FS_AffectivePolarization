# install.packages("swissparl")

library(tidyverse)
library(swissparl)

# testing 
get_tables()
get_variables("Business")
demo_business <- get_glimpse("Business")

# #################################################################################
# functions
# Function to download and save business data by session, with added robustness and delay
get_business_buffered <- function(session_id) {
  folder <- "business_50_51"
  if(!dir.exists(folder)) dir.create(folder)
  
  try({
    dt <- swissparl::get_data("Business", Language = "DE", SubmissionSession = session_id)
    if (nrow(dt) > 0) { # Check if data is non-empty
      file_path <- file.path(folder, paste0(session_id, ".rds"))
      saveRDS(dt, file_path)
    } else {
      message("No data for session: ", session_id)
    }
  }, silent = TRUE) # Silence the errors, replace with error logging if necessary
  
  Sys.sleep(3) # Pause for 3 seconds before making the next API call
}

get_voting_buffered <- function(session_id) {
  folder <- "voting_50_51"
  if(!dir.exists(folder)) dir.create(folder)
  
  try({
    dt <- swissparl::get_data("Voting", Language = "DE", IdSession = session_id)
    if (nrow(dt) > 0) { # Check if data is non-empty
      file_path <- file.path(folder, paste0(session_id, ".rds"))
      saveRDS(dt, file_path)
    } else {
      message("No data for session: ", session_id)
    }
  }, silent = TRUE) # Silence the errors, replace with error logging if necessary
  
  Sys.sleep(3) # Pause for 3 seconds before making the next API call
}

# #################################################################################
# Retrieve session data for legislative periods 50 and 51
sessions <- get_data("Session", Language = "DE", LegislativePeriodNumber = c(50,51))

# #################################################################################
# fetching all business of 50 and 51 legislative period
# Apply function to all session IDs from both periods and save as RDS files
# (only run once; will not change)

# walk(sessions$ID, get_business_buffered)

# the ones missing in 50 (once again)
# get_business_buffered(5002)
# get_business_buffered(5014)
# get_business_buffered(5015)
# get_business_buffered(5016)
# (none missing in 51)

# combine again
businesses <- map_dfr(list.files("business_50_51", full.names = T), readRDS)
# save them all
saveRDS(businesses, "data/businesses.rds", )

# #################################################################################
# fetching all voting of 50 and 51 legislative period 
# (only run once; will not change)
# walk(c(sessions50$ID, sessions51$ID), get_voting_buffered)

# combine again
votings <- map_dfr(list.files("voting_50_51", full.names = T), readRDS)
# save them all
saveRDS(votings, "data/votings.rds")

# #################################################################################
# fetching all vote(s) of 50 and 51 legislative period 
votes <- get_data("Vote", Language = "DE", IdLegislativePeriod = c(50, 51))
saveRDS(votes, "data/votes.rds")

# #################################################################################
# more info only motions
motions <- businesses %>% 
  filter(BusinessTypeName == "Motion") %>% 
  distinct(ID, .keep_all = TRUE)

# roles <- get_data("BusinessRole", Language = "DE", BusinessNumber = motions$ID)
saveRDS(roles, "data/roles.rds")

# statuses <- get_data("BusinessStatus", Language ="DE", BusinessNumber = motions$ID)
saveRDS(statuses, "data/statuses.rds")

# members_council <- get_data("MemberCouncil", Language = "DE", ID = roles$MemberCouncilNumber)
saveRDS(members_council, "data/members_council.rds")

