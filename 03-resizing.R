library(tidyverse)

motions <- readRDS("data/motions_final.rds")

# #################################################################################
# number of co-signatories per party
# only the ones with a vote
motions_with_vote <- motions %>% 
  # drop all motions without vote
  filter(!is.na(yes_votes))

# #################################################################################
# making a small version
motions_with_vote_small <- motions_with_vote %>% 
  select(
    -number_of_cosign_BDP,
    -`number_of_cosign_M-E`,
    -number_of_cosign_,
    -number_of_cosign_MCG,
    -number_of_cosign_BastA,
    -number_of_cosign_EVP,
    -number_of_cosign_LDP,
    -`number_of_cosign_csp-ow`,
    -number_of_cosign_Lega,
    -number_of_cosign_CSPO,
    -number_of_cosign_EàG,
    -number_of_cosign_Al,
    -BusinessShortNumber,
    -BusinessType,
    -`MotionStatus_Abschreibungsantrag liegt vor`,
    -`MotionStatus_In Kommission des Ständerats`,
    -`MotionStatus_Von beiden Räten behandelt`,
    -`MotionStatus_Zugewiesen an die behandelnde Kommission`,
    -`MotionStatus_In Kommission des Nationalrats`,
    -`MotionStatus_An die Kommission zur Vorberatung zugewiesen`,
    -`MotionStatus_In Nationalrat geplant`,
    -`MotionStatus_In Ständerat geplant`,
    -`MotionStatus_Überwiesen an das Ratsbüro`,
    -`MotionStatus_Beratung in Kommission des Ständerates abgeschlossen`,
    -`MotionStatus_Beratung in Kommission des Nationalrates abgeschlossen`,
    -`MotionStatus_Stellungnahme zum Vorstoss liegt vor`,
    -MotionStatus_Eingereicht,
    -MotionStatus_Angemeldet,
    -`person_ids_Sprecher(-in)`,
    -`person_ids_Bekämpfer(-in)`,
    -`person_ids_Übernehmer(-in)`,
    -`person_names_Sprecher(-in)`,
    -`person_names_Bekämpfer(-in)`,
    -`person_names_Übernehmer(-in)`,
    -`parties_Sprecher(-in)`,
    -`parties_Bekämpfer(-in)`,
    -`parties_Übernehmer(-in)`,
    -`cantons_Sprecher(-in)`,
    -`cantons_Bekämpfer(-in)`,
    -`cantons_Übernehmer(-in)`,
    -`genders_Sprecher(-in)`,
    -`genders_Bekämpfer(-in)`,
    -`genders_Übernehmer(-in)`,
    -`councils_Sprecher(-in)`,
    -`councils_Bekämpfer(-in)`,
    -`councils_Übernehmer(-in)`
  )

# only parties and results
motions_polarization <- motions_with_vote_small %>% 
  select(
    BusinessID,
    BusinessTitle,
    SubmittedText,
    TagNames,
    SubmittedBy,
    SubmissionDate,
    SubmissionCouncil,
    SubmissionLegislativePeriod,
    ReasonText,
    FederalCouncilResponseText,
    `parties_Urheber(-in)`,
    number_of_cosign_CVP,
    `number_of_cosign_FDP-Liberale`,
    number_of_cosign_GRÜNE,
    number_of_cosign_SP,
    number_of_cosign_SVP,
    number_of_cosign_glp,
    number_of_cosign_EDU,
    FederalCouncilProposalText,
    yes_votes,
    no_votes,
    absent_votes,
    withholding_votes,
    passed,
    `councils_Mitunterzeichner(-in)`,
    `parties_Mitunterzeichner(-in)`,
    starts_with("yes_votes_")
  ) %>% 
  mutate(multiple_councils = grepl("NR", `councils_Mitunterzeichner(-in)`) & 
           grepl("SR", `councils_Mitunterzeichner(-in)`))

# add party from author to co signatory column
motions_polarization <- motions_polarization %>% 
  mutate(`parties_Mitunterzeichner(-in)` = str_c(`parties_Urheber(-in)`, "; ", `parties_Mitunterzeichner(-in)`))

# clean up party names
motions_polarization <- motions_polarization %>%
  mutate(`parties_Urheber(-in)` = case_when(
    `parties_Urheber(-in)` %in% c("GLP", "glp") ~ "GLP",
    `parties_Urheber(-in)` %in% c("CVP", "BDP", "Mitte", "M-E") ~ "Mitte",
    `parties_Urheber(-in)` %in% c("FDP", "FDP-Liberale") ~ "FDP",
    TRUE ~ `parties_Urheber(-in)`
  ))

# only motions from the big parties GRÜNE, SP, GLP, CVP, FDP, SVP
motions_polarization <- motions_polarization %>% 
  filter(`parties_Urheber(-in)` %in% c("GRÜNE", "SP", "GLP", "Mitte", "FDP", "SVP"))

# define involved parties (submitting or 2 or more co-signatories)
motions_polarization <- motions_polarization %>%
  mutate(
    involved_parties = paste0(
      `parties_Urheber(-in)`,
      if_else(number_of_cosign_CVP >= 2 & `parties_Urheber(-in)` != "Mitte", ", Mitte", ""),
      if_else(`number_of_cosign_FDP-Liberale` >= 2 & `parties_Urheber(-in)` != "FDP", ", FDP", ""),
      if_else(number_of_cosign_GRÜNE >= 2 & `parties_Urheber(-in)` != "GRÜNE", ", GRÜNE", ""),
      if_else(number_of_cosign_SP >= 2 & `parties_Urheber(-in)` != "SP", ", SP", ""),
      if_else(number_of_cosign_SVP >= 2 & `parties_Urheber(-in)` != "SVP", ", SVP", ""),
      if_else(number_of_cosign_glp >= 2 & `parties_Urheber(-in)` != "GLP", ", GLP", "")
    )
  ) %>%
  # Remove duplicates and clean up the string
  mutate(involved_parties = str_split(involved_parties, ", ")) %>%
  mutate(involved_parties = sapply(involved_parties, function(x) paste(unique(x), collapse = ", ")))


saveRDS(motions_polarization, "data/motions_polarization.rds")

