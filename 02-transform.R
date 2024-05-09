library(tidyverse)

# loading
businesses <- readRDS("data/businesses.rds")
votings <- readRDS("data/votings.rds")
votes <- readRDS("data/votes.rds")
members_council <- readRDS("data/members_council.rds")
roles <- readRDS("data/roles.rds")
statuses <- readRDS("data/statuses.rds")


# #################################################################################
# summarizing votings and joining it with votes
votings_summarized <- votings %>%
  group_by(IdVote) %>%
  summarise(
    yes_votes = sum(DecisionText == "Ja", na.rm = TRUE),
    no_votes = sum(DecisionText == "Nein", na.rm = TRUE),
    absent_votes = sum(DecisionText == "Hat nicht teilgenommen", na.rm = TRUE),
    withholding_votes = sum(DecisionText == "Enthaltung", na.rm = TRUE),
    president_votes = sum(DecisionText == "Die Präsidentin/der Präsident stimmt nicht", na.rm = TRUE),
    # yes_votes per party
    yes_votes_green = sum(DecisionText == "Ja" & ParlGroupName == "Grüne Fraktion", na.rm = TRUE),
    yes_votes_sp = sum(DecisionText == "Ja" & ParlGroupName == "Sozialdemokratische Fraktion", na.rm = TRUE),
    yes_votes_glp = sum(DecisionText == "Ja" & ParlGroupName == "Grünliberale Fraktion", na.rm = TRUE),
    yes_votes_mitte = sum(DecisionText == "Ja" & ParlGroupName == "Die Mitte-Fraktion. Die Mitte. EVP.", na.rm = TRUE) + 
      sum(DecisionText == "Ja" & ParlGroupName == "CVP-Fraktion", na.rm = TRUE) +
      sum(DecisionText == "Ja" & ParlGroupName == "Fraktion BD", na.rm = TRUE),
    yes_votes_fdp = sum(DecisionText == "Ja" & ParlGroupName == "FDP-Liberale Fraktion", na.rm = TRUE),
    yes_votes_svp = sum(DecisionText == "Ja" & ParlGroupName == "Fraktion der Schweizerischen Volkspartei", na.rm = TRUE)
  )

# joining voting and vote
votes_with_results <- votes %>% 
  left_join(votings_summarized, by = c("ID" = "IdVote"))

# adding passed column
votes_with_results <- votes_with_results %>%
  mutate(passed = ifelse(yes_votes > no_votes, "1", "0"))

# #################################################################################
# only motions, no duplicates - for further analyzes
motions <- businesses %>% 
  filter(BusinessTypeName == "Motion")

# #################################################################################
# motion statuses
motion_statuses <- left_join(motions, statuses, by = c("ID" = "BusinessNumber"))

# distinct
motion_statuses_unique <- motion_statuses %>%
  distinct(ID, BusinessStatusName, BusinessStatusDate.y, .keep_all = TRUE)

# aggregate to get the earliest date for each status per business
motion_statuses_aggregated <- motion_statuses_unique %>%
  group_by(ID, BusinessStatusName) %>%
  summarise(StatusDate = min(BusinessStatusDate.y), .groups = 'drop')

# widening
motion_statuses_wide <- motion_statuses_aggregated %>% 
  pivot_wider(
    names_from = BusinessStatusName,
    values_from = StatusDate,
    names_prefix = "MotionStatus_",
    values_fill = list(BusinessStatusDate.y = NA)
  )

# join wide and unique

# #################################################################################
# motion roles

# roles and member_council
roles_info <- left_join(roles, members_council, by = c("MemberCouncilNumber" = "ID"))

# remove duplicates
roles_info <- roles_info %>% 
  #distinct()
  distinct(BusinessNumber, PersonNumber, RoleName, .keep_all = TRUE)

# summary of roles_info for motions
summary_roles_info <- roles_info %>%
  group_by(BusinessNumber, RoleName) %>%
  summarise(
    person_ids = paste(PersonNumber, sep=", ", collapse="; "),
    person_names = paste(FirstName, LastName, sep=" ", collapse="; "),  # Fixed sep to combine names properly
    parties = paste(PartyAbbreviation, sep=", ", collapse="; "),
    cantons = paste(CantonAbbreviation, sep=", ", collapse="; "),
    genders = paste(GenderAsString, sep=", ", collapse="; "),
    councils = paste(CouncilAbbreviation, sep=", ", collapse="; "),
    .groups = "drop"
  )

# wider
summary_roles_info_wide <- summary_roles_info %>%
  pivot_wider(
    names_from = RoleName,
    values_from = c(person_ids, person_names, parties, cantons, genders, councils),
    values_fill = list(person_ids = "", person_names = "", parties = "", cantons = "", genders = "", councils = "")  # Fill with empty strings if no data available
  )

# #################################################################################
# joining 

# join status aggregated wide with motions
motions_enriched <- left_join(motions, motion_statuses_wide, by = c("ID" = "ID"))

# join roles summary with motions
motions_enriched2 <- left_join(motions_enriched, summary_roles_info_wide, by = c("ID" = "BusinessNumber"))

# join motions_enriched with votes_with_results
motions_raw <- left_join(motions_enriched2, votes_with_results, by = c("ID" = "BusinessNumber"))

# #################################################################################
# finalizing

# select
motions_final <- motions_raw %>% 
  select(BusinessID = ID, 
         BusinessShortNumber = BusinessShortNumber.x,
         BusinessType = BusinessTypeName,
         BusinessTitle,
         BusinessStatusText,
         SubmittedText,
         ReasonText,
         FederalCouncilResponseText,
         FederalCouncilProposalCode = FederalCouncilProposal,
         FederalCouncilProposalText,
         ResponsibleDepartment = ResponsibleDepartmentAbbreviation,
         SubmittedBy,
         SubmissionDate,
         SubmissionCouncil = SubmissionCouncilAbbreviation,
         SubmissionSession,
         SubmissionLegislativePeriod,
         FirstCouncil = FirstCouncil1Abbreviation,
         TagNames,
         starts_with("MotionStatus_"),
         starts_with("person_"),
         starts_with("parties_"),
         starts_with("cantons_"),
         starts_with("genders_"),
         starts_with("councils_"),
         starts_with("yes_votes_"),
         MeaningYes,
         MeaningNo,
         VoteEnd,
         VoteID = ID.y,
         yes_votes,
         no_votes,
         absent_votes,
         withholding_votes,
         president_votes,
         passed)
  # drop all motions without vote
  # filter(!is.na(yes_votes))

motions_final <- motions_final %>% 
  distinct(BusinessID, .keep_all = TRUE)

# #################################################################################
# get number of co-signatories
motions_final <- motions_final %>% 
  mutate(number_of_cosignatories = str_count(`person_ids_Mitunterzeichner(-in)`, ";") + 1)

# #################################################################################
# number of co-signatories per party
motions_final <- motions_final %>% 
  # separate parties into different rows
  separate_rows(`parties_Mitunterzeichner(-in)`, sep = ";\\s*") %>% 
  # count each party per original row
  count(BusinessID, `parties_Mitunterzeichner(-in)`) %>% 
  # pivot
  pivot_wider(
    names_from = `parties_Mitunterzeichner(-in)`,
    values_from = n,
    names_prefix = "number_of_cosign_",
    values_fill = list(n = 0)
  ) %>% 
  # join back to original
  right_join(motions_final, by = "BusinessID")

# #################################################################################
# number of co-signatories per gender
motions_final <- motions_final %>% 
  # separate parties into different rows
  separate_rows(`genders_Mitunterzeichner(-in)`, sep = ";\\s*") %>% 
  # count each party per original row
  count(BusinessID, `genders_Mitunterzeichner(-in)`) %>% 
  # pivot
  pivot_wider(
    names_from = `genders_Mitunterzeichner(-in)`,
    values_from = n,
    names_prefix = "number_of_cosign_gender_",
    values_fill = list(n = 0)
  ) %>% 
  # join back to original
  right_join(motions_final, by = "BusinessID")

# #################################################################################
# saving
saveRDS(motions_final, "data/motions_final.rds")







