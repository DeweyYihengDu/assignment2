library(tidyverse)
library(metafor)

# read dataset from public repo
data <- read_csv('data/new_data.csv', show_col_types = FALSE)

# compute summary statistics for each species
summaries <- data %>%
  group_by(species, treatment) %>%
  summarise(mean_activity = mean(activity, na.rm = TRUE),
            sd_activity = sd(activity, na.rm = TRUE),
            n = sum(!is.na(activity)), .groups='drop')

# pivot to wide format
wide <- summaries %>% pivot_wider(names_from = treatment, values_from = c(mean_activity, sd_activity, n))

# remove rows with missing values
wide <- wide %>% drop_na()

# compute log response ratio LnRR of OA vs control
calc <- escalc(measure='ROM',
               m1i = mean_activity_control, m2i = mean_activity_CO2,
               sd1i = sd_activity_control, sd2i = sd_activity_CO2,
               n1i = n_control, n2i = n_CO2,
               var.names=c('LnRR','v'), data=wide)

# fit random-effects model
model <- rma(LnRR, v, data=calc, method='REML')
print(summary(model))

