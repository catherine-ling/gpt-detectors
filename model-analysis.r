knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(readr)
library(tidyverse)
library(broom)
library(janitor)
library(tibble)

# Load data
detectors <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-07-18/detectors.csv")

human_essays_only <- detectors %>%
  clean_names() %>%
  filter(kind == "Human") %>%
  mutate(
    correct = kind == pred_class
  )

knitr::kable(head(human_essays_only))

model_correct <- glm(correct ~ detector + native + detector:native,
                     family = "binomial",
                     data = human_essays_only)
knitr::kable(tidy(model_correct) %>% mutate(odds_ratio = exp(estimate)), digits = 3)

newdata_prob <- expand.grid(
  detector = unique(human_essays_only$detector),
  native = c("No", "Yes")
)

newdata_prob$prob <- predict(model_correct, newdata = newdata_prob, type = "response")
newdata_prob$native_label <- ifelse(newdata_prob$native == "Yes", "Native", "Non-Native")

ggplot(newdata_prob, aes(x = detector, y = prob, fill = native_label)) +
  geom_col(position = "dodge") +
  labs(
    title = "Figure 1: Probability of Correct Classification by Native Status",
    x = "Detector",
    y = "Predicted Probability",
    fill = "Writer Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal(base_family = "serif")

ggplot(human_essays_only %>%
         mutate(native_binary = if_else(native == "Yes", 1, 0)),
       aes(x = native_binary, y = as.numeric(correct))) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.2, color = "black") +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE,
              color = "blue",
              size = 1.2) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Non-Native", "Native")) +
  labs(
    title = "Figure 2: Correct Classification by Native English Status",
    x = "Writer Type",
    y = "Correct (1 = Yes, 0 = No)"
  ) +
  theme_minimal(base_family = "serif")

coef_table <- summary(model_correct)$coefficients

interaction_terms <- as_tibble(coef_table, rownames = "term") %>%
  filter(grepl(":native", term)) %>%
  transmute(
    term,
    estimate = Estimate,
    p_value = `Pr(>|z|)`,
    odds_ratio = exp(estimate)
  )

knitr::kable(interaction_terms)




