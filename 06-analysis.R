library(tidyverse)

# motions_polarization <- readRDS("data/motions_polarization_with_lr_ap.rds")
motions_polarization <- readRDS("data/motions_polarization_with_lr_ap_inverted.rds")

# # #################################################################################
# summary

summary(motions_polarization)

# # #################################################################################
# control variable FederalCouncilProposalText

unique_values <- unique(motions_polarization$FederalCouncilProposalText)
print(unique_values)

# Assuming 'your_dataframe' is your dataframe and 'FederalCouncilProposalText' is your column
motions_polarization$FederalCouncilProposalEncoded <- ifelse(
  motions_polarization$FederalCouncilProposalText == "Der Bundesrat beantragt die Annahme der Motion.", 1, 0)

# # #################################################################################
# resave for assumptions
saveRDS(motions_polarization, "data/motions_polarization_with_lr_ap_inverted_controls.rds")


# # #################################################################################
# testing data with sotomo

# lm1 <- lm(motions_polarization$yes_votes ~ motions_polarization$lr_distance_normalized)
# summary(lm1)
# 
# motions_polarization %>%
#   ggplot(aes(x = lr_distance_normalized, y = yes_votes, color = passed)) +
#   geom_point(alpha = 0.7, size = 3) +
#   geom_smooth(method = "lm", se = TRUE,
#               aes(group = 1),
#               color = "black") +
#   theme_minimal() +
#   labs(
#     x = "Normalized LR Distance",
#     y = "'Yes' votes",
#     title = "Yes Votes ~ Normalized LR Distance",
#     color = "failed/passed"
#   )
# 
# lm2 <- lm(motions_polarization$yes_votes ~ motions_polarization$ap_score_normalized)
# summary(lm2)
# 
# motions_polarization %>%
#   ggplot(aes(x = ap_score_normalized, y = yes_votes, color = passed)) +
#   geom_point(alpha = 0.7, size = 3) +
#   geom_smooth(method = "lm", se = TRUE,
#               aes(group = 1),
#               color = "black") +
#   theme_minimal() +
#   labs(
#     x = "Normalized AP Score",
#     y = "'Yes' votes",
#     title = "Yes Votes ~ Normalized AP Acore",
#     color = "failed/passed"
#   )
# 
# 
# lm3 <- lm(motions_polarization$yes_votes ~ 
#             motions_polarization$ap_score_normalized + 
#             motions_polarization$lr_distance_normalized +
#             motions_polarization$FederalCouncilProposalEncoded +
#             motions_polarization$multiple_councils)
# summary(lm3)
# 
# motions_polarization %>%
#   ggplot(aes(x = lr_distance_normalized, y = ap_score_normalized, color = passed)) +
#   geom_point(alpha = 0.7, size = 3) +
#   geom_smooth(method = "lm", se = TRUE,
#               aes(group = 1),
#               color = "black") +
#   theme_minimal() +
#   labs(
#     x = "Normalized L/R Distance",
#     y = "Normalized AP Score",
#     title = "Normalized AP Score ~ Normalized L/R Distance",
#     color = "failed/passed"
#   )

# # #################################################################################
# testing data without sotomo

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

motions_polarization %>%
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


























# # #################################################################################
# subset for migration
motions_polarization_migration <- motions_polarization %>%
  filter(str_detect(TagNames, "\\bMigration\\b"))

lm_migration <- lm(motions_polarization_migration$yes_votes ~ motions_polarization_migration$ap_score_normalized2)
summary(lm_migration)

motions_polarization_migration %>%
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

lm_migration2 <- lm(motions_polarization_migration$yes_votes ~ 
                      motions_polarization_migration$ap_score_normalized2 + 
                      motions_polarization_migration$lr_distance_normalized +
                      motions_polarization_migration$FederalCouncilProposalEncoded +
                      motions_polarization_migration$multiple_councils)

summary(lm_migration2)

# # #################################################################################
# subset for traffic
motions_polarization_traffic <- motions_polarization %>%
  filter(str_detect(TagNames, "\\bVerkehr\\b"))

lm_traffic <- lm(motions_polarization_traffic$yes_votes ~ motions_polarization_traffic$ap_score_normalized2)
summary(lm_traffic)

motions_polarization_traffic %>%
  ggplot(aes(x = ap_score_normalized2, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized AP Score",
    y = "'Yes' votes",
    title = "«Verkehr» - Yes Votes ~ Normalized AP Acore",
    color = "failed/passed"
  )

lm_traffic2 <- lm(motions_polarization_traffic$yes_votes ~ 
                      motions_polarization_traffic$ap_score_normalized2 + 
                      motions_polarization_traffic$lr_distance_normalized +
                      motions_polarization_traffic$FederalCouncilProposalEncoded +
                      motions_polarization_traffic$multiple_councils)

summary(lm_traffic2)

# # #################################################################################
# subset for healthcare
motions_polarization_health <- motions_polarization %>%
  filter(str_detect(TagNames, "\\bGesundheit\\b"))

lm_health <- lm(motions_polarization_health$yes_votes ~ motions_polarization_health$ap_score_normalized2)
summary(lm_health)

motions_polarization_health %>%
  ggplot(aes(x = ap_score_normalized2, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized AP Score",
    y = "'Yes' votes",
    title = "«Gesundheit» - Yes Votes ~ Normalized AP Acore",
    color = "failed/passed"
  )

lm_health2 <- lm(motions_polarization_health$yes_votes ~ 
                    motions_polarization_health$ap_score_normalized2 + 
                    motions_polarization_health$lr_distance_normalized +
                    motions_polarization_health$FederalCouncilProposalEncoded +
                    motions_polarization_health$multiple_councils)

summary(lm_health2)

# # #################################################################################
# subset for science
motions_polarization_science <- motions_polarization %>%
  filter(str_detect(TagNames, "\\bWissenschaft\\b"))

lm_science <- lm(motions_polarization_science$yes_votes ~ motions_polarization_science$ap_score_normalized2)
summary(lm_science)

motions_polarization_science %>%
  ggplot(aes(x = ap_score_normalized2, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized AP Score",
    y = "'Yes' votes",
    title = "«Wissenschaft» - Yes Votes ~ Normalized AP Acore",
    color = "failed/passed"
  )

lm_science2 <- lm(motions_polarization_science$yes_votes ~ 
                   motions_polarization_science$ap_score_normalized2 + 
                   motions_polarization_science$lr_distance_normalized +
                   motions_polarization_science$FederalCouncilProposalEncoded +
                   motions_polarization_science$multiple_councils)

summary(lm_science2)

# # #################################################################################
# subset for europa
motions_polarization_europa <- motions_polarization %>%
  filter(str_detect(TagNames, "\\bEuropapolitik\\b"))

lm_europa <- lm(motions_polarization_europa$yes_votes ~ motions_polarization_europa$ap_score_normalized2)
summary(lm_europa)

motions_polarization_europa %>%
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

lm_europa2 <- lm(motions_polarization_europa$yes_votes ~ 
                    motions_polarization_europa$ap_score_normalized2 + 
                    motions_polarization_europa$lr_distance_normalized +
                    motions_polarization_europa$FederalCouncilProposalEncoded +
                    motions_polarization_europa$multiple_councils)

summary(lm_europa2)

# # #################################################################################
# subset for agriculture
motions_polarization_agri <- motions_polarization %>%
  filter(str_detect(TagNames, "\\bLandwirtschaft\\b"))

lm_agri <- lm(motions_polarization_agri$yes_votes ~ motions_polarization_agri$ap_score_normalized2)
summary(lm_agri)

motions_polarization_agri %>%
  ggplot(aes(x = ap_score_normalized2, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized AP Score",
    y = "'Yes' votes",
    title = "«Landwirtschaft» - Yes Votes ~ Normalized AP Acore",
    color = "failed/passed"
  )

lm_agri2 <- lm(motions_polarization_agri$yes_votes ~ 
                   motions_polarization_agri$ap_score_normalized2 + 
                   motions_polarization_agri$lr_distance_normalized +
                   motions_polarization_agri$FederalCouncilProposalEncoded +
                   motions_polarization_agri$multiple_councils)

summary(lm_agri2)
