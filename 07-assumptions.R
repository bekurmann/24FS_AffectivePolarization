library(tidyverse)

motions_polarization <- readRDS("data/motions_polarization_with_lr_ap_inverted_controls.rds")

# # #################################################################################
# testing linear regression assumptions

# models
lm1 <- lm(motions_polarization$yes_votes ~ motions_polarization$lr_distance_normalized)
lm2 <- lm(motions_polarization$yes_votes ~ motions_polarization$ap_score_normalized)
lm3 <- lm(motions_polarization$yes_votes ~ 
            motions_polarization$ap_score_normalized + 
            motions_polarization$lr_distance_normalized +
            motions_polarization$FederalCouncilProposalEncoded +
            motions_polarization$multiple_councils)


# # #################################################################################
# linearity
par(mfrow = c(1, 3))  # Set up the plot layout

plot(lm1$fitted.values, lm1$residuals,
     main = "Linearity Check for lm1",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")

plot(lm2$fitted.values, lm2$residuals,
     main = "Linearity Check for lm2",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")

plot(lm3$fitted.values, lm3$residuals,
     main = "Linearity Check for lm3",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")


# # #################################################################################
# homoscedasticity (constant variance of residuals «Varianzhomogenität»)

par(mfrow = c(1, 3))  # Set up the plot layout

plot(lm1$fitted.values, lm1$residuals,
     main = "Homoscedasticity Check for lm1",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")

plot(lm2$fitted.values, lm2$residuals,
     main = "Homoscedasticity Check for lm2",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")

plot(lm3$fitted.values, lm3$residuals,
     main = "Homoscedasticity Check for lm3",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")


# # #################################################################################
# normality of residuals
par(mfrow = c(1, 3))  # Set up the plot layout

qqnorm(lm1$residuals, main = "Q-Q Plot for lm1")
qqline(lm1$residuals, col = "red")

qqnorm(lm2$residuals, main = "Q-Q Plot for lm2")
qqline(lm2$residuals, col = "red")

qqnorm(lm3$residuals, main = "Q-Q Plot for lm3")
qqline(lm3$residuals, col = "red")

# Histograms
hist(lm1$residuals, breaks = 20, main = "Histogram of lm1 Residuals", xlab = "Residuals", col = "lightblue")
hist(lm2$residuals, breaks = 20, main = "Histogram of lm2 Residuals", xlab = "Residuals", col = "lightblue")
hist(lm3$residuals, breaks = 20, main = "Histogram of lm3 Residuals", xlab = "Residuals", col = "lightblue")


# Shapiro-Wilk Test
shapiro.test(lm1$residuals)
shapiro.test(lm2$residuals)
shapiro.test(lm3$residuals)


# # #################################################################################
# multicollinearity (only lm3)

# Function to calculate VIF for a single independent variable
calculate_vif <- function(model) {
  vif_values <- NULL
  for (var in names(model$model)[-1]) {
    formula <- as.formula(paste(var, "~ ."))
    r_squared <- summary(lm(formula, data = model$model))$r.squared
    vif_values <- c(vif_values, 1 / (1 - r_squared))
  }
  names(vif_values) <- names(model$model)[-1]
  return(vif_values)
}

# Calculate VIF for lm3
vif_lm3 <- calculate_vif(lm3)
print(vif_lm3)

# Subset the independent variables used in lm3
independent_vars <- motions_polarization %>%
  select(ap_score_normalized, lr_distance_normalized, FederalCouncilProposalEncoded, multiple_councils)

# Calculate and print the correlation matrix
cor_matrix <- cor(independent_vars, use = "complete.obs")
print(cor_matrix)



