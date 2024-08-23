library(tidyverse)

# motions_polarization <- readRDS("data/motions_polarization_with_lr_ap.rds")
motions_polarization <- readRDS("data/motions_polarization_with_lr_ap_inverted.rds")

# # #################################################################################
# control variable FederalCouncilProposalText

unique_values <- unique(motions_polarization$FederalCouncilProposalText)
print(unique_values)

# Encoding 'FederalCouncilProposalText' to 0/1
motions_polarization$FederalCouncilProposalEncoded <- ifelse(
  motions_polarization$FederalCouncilProposalText == "Der Bundesrat beantragt die Annahme der Motion." | 
    motions_polarization$FederalCouncilProposalText == "Das Büro beantragt die Ablehnung der Motion.", TRUE, FALSE)

# # #################################################################################
# resave for assumptions
saveRDS(motions_polarization, "data/motions_polarization_with_lr_ap_inverted_controls.rds")

# # #################################################################################
# variable summaries
# # #################################################################################
# with sotomo
summary(motions_polarization$yes_votes)
summary(motions_polarization$lr_distance_normalized)
summary(motions_polarization$ap_score_normalized)
summary(motions_polarization$FederalCouncilProposalEncoded)
summary(motions_polarization$multiple_councils)

# without sotomo
summary(motions_polarization$ap_score_normalized2)

# # #################################################################################
# testing data without sotomo
# # #################################################################################

lm1 <- lm(motions_polarization$yes_votes ~ motions_polarization$lr_distance_normalized)
summary(lm1)

lm2 <- lm(motions_polarization$yes_votes ~ motions_polarization$ap_score_normalized2)
summary(lm2)

lm3 <- lm(motions_polarization$yes_votes ~ 
            motions_polarization$ap_score_normalized2 + 
            motions_polarization$lr_distance_normalized +
            motions_polarization$FederalCouncilProposalEncoded +
            motions_polarization$multiple_councils)
summary(lm3)


# # #################################################################################
# testing data with sotomo
# # #################################################################################
lm3_sotomo <- lm(motions_polarization$yes_votes ~
                   motions_polarization$ap_score_normalized +
                   motions_polarization$lr_distance_normalized +
                   motions_polarization$FederalCouncilProposalEncoded +
                   motions_polarization$multiple_councils)
summary(lm3_sotomo)

# # #################################################################################
# plots
# # #################################################################################

# # #################################################################################
# yes_votes ~ ap_score (without sotomo)
yesvotes_apscore <- motions_polarization %>%
  ggplot(aes(x = ap_score_normalized2, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized AP Score",
    y = "'Yes' votes",
    title = "Yes Votes ~ Normalized AP Acore (without Sotomo)",
    color = "failed/passed"
  )

# save
ggsave(filename = "img/yesvotes_apscore.png", plot = yesvotes_apscore, width = 8, height = 6, dpi = 300)

# # #################################################################################
# yes_votes ~ l/r distance (without sotomo)
yesvotes_lrdistance <- motions_polarization %>%
  ggplot(aes(x = lr_distance_normalized, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized L/R Distance",
    y = "'Yes' votes",
    title = "Yes Votes ~ Normalized L/R Distance",
    color = "failed/passed"
  )

# save
ggsave(filename = "img/yesvotes_lrdistance.png", plot = yesvotes_lrdistance, width = 8, height = 6, dpi = 300)

# # #################################################################################
# yes_votes ~ multiple councils
yesvotes_multiplecouncils <- motions_polarization %>%
  ggplot(aes(x = multiple_councils, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Multiple Councils",
    y = "'Yes' votes",
    title = "Yes Votes ~ Multiple Councils",
    color = "failed/passed"
  )

# save
ggsave(filename = "img/yesvotes_multiplecouncils.png", plot = yesvotes_multiplecouncils, width = 8, height = 6, dpi = 300)

# # #################################################################################
# yes_votes ~ federal council proposal
yesvotes_federalproposal <- motions_polarization %>%
  ggplot(aes(x = FederalCouncilProposalEncoded, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Federal Council Proposal",
    y = "'Yes' votes",
    title = "Yes Votes ~ Federal Council Proposal",
    color = "failed/passed"
  )

# save
ggsave(filename = "img/yesvotes_federalproposal.png", plot = yesvotes_federalproposal, width = 8, height = 6, dpi = 300)

# # #################################################################################
# lr_distance ~ ap_score (without sotomo)
lrdistance_apscore <- motions_polarization %>%
  ggplot(aes(x = lr_distance_normalized, y = ap_score_normalized2, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized L/R Distance",
    y = "Normalized AP Score",
    title = "Normalized AP Score ~ Normalized L/R Distance",
    color = "failed/passed"
  )

# save
ggsave(filename = "img/lrdistance_apscore.png", plot = lrdistance_apscore, width = 8, height = 6, dpi = 300)


# # #################################################################################
# all subsets without sotomo (matrix2)
# # #################################################################################

# subset for migration
motions_polarization_migration <- motions_polarization %>%
  filter(str_detect(TagNames, "\\bMigration\\b"))

lm_migration2 <- lm(motions_polarization_migration$yes_votes ~ 
                      motions_polarization_migration$ap_score_normalized2 + 
                      motions_polarization_migration$lr_distance_normalized +
                      motions_polarization_migration$FederalCouncilProposalEncoded +
                      motions_polarization_migration$multiple_councils)

summary(lm_migration2)

# plot
yesvotes_apscore_migration <- motions_polarization_migration %>%
  ggplot(aes(x = ap_score_normalized2, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized AP Score",
    y = "'Yes' votes",
    title = "«Migration» - Yes Votes ~ Normalized AP Acore",
    color = "failed/passed"
  )

# save
ggsave(filename = "img/yesvotes_apscore_migration.png", plot = yesvotes_apscore_migration, width = 8, height = 6, dpi = 300)


# # #################################################################################
# subset for traffic
motions_polarization_traffic <- motions_polarization %>%
  filter(str_detect(TagNames, "\\bVerkehr\\b"))

lm_traffic2 <- lm(motions_polarization_traffic$yes_votes ~ 
                    motions_polarization_traffic$ap_score_normalized2 + 
                    motions_polarization_traffic$lr_distance_normalized +
                    motions_polarization_traffic$FederalCouncilProposalEncoded +
                    motions_polarization_traffic$multiple_councils)

summary(lm_traffic2)

#plot
yesvotes_apscore_trafic <- motions_polarization_traffic %>%
  ggplot(aes(x = ap_score_normalized2, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized AP Score",
    y = "'Yes' votes",
    title = "«Traffic» - Yes Votes ~ Normalized AP Acore",
    color = "failed/passed"
  )

# save
ggsave(filename = "img/yesvotes_apscore_trafic.png", plot = yesvotes_apscore_trafic, width = 8, height = 6, dpi = 300)

# # #################################################################################
# subset for healthcare
motions_polarization_health <- motions_polarization %>%
  filter(str_detect(TagNames, "\\bGesundheit\\b"))

lm_health2 <- lm(motions_polarization_health$yes_votes ~ 
                   motions_polarization_health$ap_score_normalized2 + 
                   motions_polarization_health$lr_distance_normalized +
                   motions_polarization_health$FederalCouncilProposalEncoded +
                   motions_polarization_health$multiple_councils)

summary(lm_health2)

# plot
yesvotes_apscore_healthcare <- motions_polarization_health %>%
  ggplot(aes(x = ap_score_normalized2, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized AP Score",
    y = "'Yes' votes",
    title = "«Healthcare» - Yes Votes ~ Normalized AP Acore",
    color = "failed/passed"
  )

# save
ggsave(filename = "img/yesvotes_apscore_healthcare.png", plot = yesvotes_apscore_healthcare, width = 8, height = 6, dpi = 300)

# # #################################################################################
# subset for science
motions_polarization_science <- motions_polarization %>%
  filter(str_detect(TagNames, "\\bWissenschaft\\b"))

lm_science2 <- lm(motions_polarization_science$yes_votes ~ 
                    motions_polarization_science$ap_score_normalized2 + 
                    motions_polarization_science$lr_distance_normalized +
                    motions_polarization_science$FederalCouncilProposalEncoded +
                    motions_polarization_science$multiple_councils)

summary(lm_science2)

# plot
yesvotes_apscore_science <- motions_polarization_science %>%
  ggplot(aes(x = ap_score_normalized2, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized AP Score",
    y = "'Yes' votes",
    title = "«Science» - Yes Votes ~ Normalized AP Acore",
    color = "failed/passed"
  )

# save
ggsave(filename = "img/yesvotes_apscore_science.png", plot = yesvotes_apscore_science, width = 8, height = 6, dpi = 300)

# # #################################################################################
# subset for europa
motions_polarization_europa <- motions_polarization %>%
  filter(str_detect(TagNames, "\\bEuropapolitik\\b"))

lm_europa2 <- lm(motions_polarization_europa$yes_votes ~ 
                   motions_polarization_europa$ap_score_normalized2 + 
                   motions_polarization_europa$lr_distance_normalized +
                   motions_polarization_europa$FederalCouncilProposalEncoded +
                   motions_polarization_europa$multiple_councils)

summary(lm_europa2)

# plot
yesvotes_apscore_europa <- motions_polarization_europa %>%
  ggplot(aes(x = ap_score_normalized2, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized AP Score",
    y = "'Yes' votes",
    title = "«Europa» - Yes Votes ~ Normalized AP Acore",
    color = "failed/passed"
  )

# save
ggsave(filename = "img/yesvotes_apscore_europa.png", plot = yesvotes_apscore_europa, width = 8, height = 6, dpi = 300)

# # #################################################################################
# subset for agriculture
motions_polarization_agri <- motions_polarization %>%
  filter(str_detect(TagNames, "\\bLandwirtschaft\\b"))

lm_agri2 <- lm(motions_polarization_agri$yes_votes ~ 
                 motions_polarization_agri$ap_score_normalized2 + 
                 motions_polarization_agri$lr_distance_normalized +
                 motions_polarization_agri$FederalCouncilProposalEncoded +
                 motions_polarization_agri$multiple_councils)

summary(lm_agri2)

# plot
yesvotes_apscore_agriculture <- motions_polarization_agri %>%
  ggplot(aes(x = ap_score_normalized2, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized AP Score",
    y = "'Yes' votes",
    title = "«Agriculture» - Yes Votes ~ Normalized AP Acore",
    color = "failed/passed"
  )

# save
ggsave(filename = "img/yesvotes_apscore_agriculture.png", plot = yesvotes_apscore_agriculture, width = 8, height = 6, dpi = 300)
