library(tidyverse)

motions <- readRDS("data/motions_polarization_with_lr_ap.rds")


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
    x = "Normalized Outside LR Distance",
    y = "'Yes' votes",
    title = "Scatter Plot of normalized LR Distance vs. Yes Votes",
    color = "failed/passed"
  )
