library(tidyverse)

motions_polarization <- readRDS("data/motions_polarization_with_lr_ap.rds")

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
# testing

lm1 <- lm(motions_polarization$yes_votes ~ motions_polarization$lr_distance_normalized)
summary(lm1)

motions_polarization %>%
  ggplot(aes(x = lr_distance_normalized, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized LR Distance",
    y = "'Yes' votes",
    title = "Yes Votes ~ Normalized LR Distance",
    color = "failed/passed"
  )

lm2 <- lm(motions_polarization$yes_votes ~ motions_polarization$ap_score_normalized)
summary(lm2)

motions_polarization %>%
  ggplot(aes(x = ap_score_normalized, y = yes_votes, color = passed)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = TRUE,
              aes(group = 1),
              color = "black") +
  theme_minimal() +
  labs(
    x = "Normalized AP Score",
    y = "'Yes' votes",
    title = "Yes Votes ~ Normalized AP Acore",
    color = "failed/passed"
  )


lm3 <- lm(motions_polarization$yes_votes ~ 
            motions_polarization$ap_score_normalized + 
            motions_polarization$lr_distance_normalized +
            motions_polarization$FederalCouncilProposalEncoded +
            motions_polarization$multiple_councils)
summary(lm3)

motions_polarization %>%
  ggplot(aes(x = lr_distance_normalized, y = ap_score_normalized, color = passed)) +
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

