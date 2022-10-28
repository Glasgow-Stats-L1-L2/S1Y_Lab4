# R-script for S1Y Lab 4

library(tidyverse)

### PART 1

global_monitor <- tibble(
  scientist_work = c(rep("Benefits", 80000), rep("Doesn't benefit", 20000)))

head(global_monitor)

global_monitor %>%
  count(scientist_work) %>%  # This creates a new variable called "n" containing the counts of the different values in "scientist_work"
  mutate(p = n /sum(n)) # This creates a new variable called "p" containing the proportions of the different values in "scientist_work"


ggplot(global_monitor, aes(x = scientist_work)) +
  geom_bar(fill = "lightblue", width = 0.5) +
  labs(x = "", y = "",
       title = "Population counts of beliefs in the work scientists do") +
  coord_flip()

sample1 <- global_monitor %>%
  sample_n(50)

sample1 %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n))

sample_props50 <- global_monitor %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n)) %>%
  filter(scientist_work == "Doesn't benefit")

ggplot(data = sample_props50, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.05, fill = "seagreen3") +
  labs(x = "p_hat (Doesn't benefit)",
       title = "Sampling distribution of p_hat",
       subtitle = "Sample size = 50, Number of samples = 15000")


sample_props50 <- global_monitor %>%
  rep_sample_n(size = 50, reps = 500, replace = TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n)) %>%
  filter(scientist_work == "Doesn't benefit")

ggplot(data = sample_props50, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.05, fill = "seagreen3") +
  labs(x = "p_hat (Doesn't benefit)",
       title = "Sampling distribution of p_hat",
       subtitle = "Sample size = 50")



sample_props <- global_monitor %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n)) %>%
  filter(scientist_work == "Doesn't benefit")

ggplot(data = sample_props, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.02, fill = "seagreen3") +
  xlim(0, 1) +
  labs(x = "p_hat (Doesn't benefit)",
       title = "Sampling distribution of p_hat",
       subtitle = "Trying different sample sizes")

##########################################################################
### PART 2
us_adults <- tibble(
  climate_change_affects = c(rep("Yes", 62000), rep("No", 38000)))

us_adults %>%
  count(climate_change_affects) %>%
  mutate(p = n /sum(n))

ggplot(us_adults, aes(x = climate_change_affects)) +
  geom_bar(fill = "lightblue", width = 0.5) +
  labs(x = "",
       y = "",
       title = "Do you think climate change is affecting your local community?" ) +
  coord_flip()

sample <- us_adults %>%
  sample_n(60)

sample %>%
  count(climate_change_affects) %>%
  mutate(p_hat = n /sum(n))



sample <- us_adults %>%
  sample_n(60)

sample_pro <- proportions(table(sample))
sample_pro <- sample_pro[2]

sample_se <- sqrt(sample_pro * (1 - sample_pro) / 60)

lower_vector <- sample_pro - 1.96 * sample_se / sqrt(60)
upper_vector <- sample_pro + 1.96 * sample_se / sqrt(60)

ci <- c(lower_vector, upper_vector)

ci



n <- 60
sample <- us_adults %>%
  sample_n(n)

sample_pro <- proportions(table(sample))
sample_pro <- sample_pro[2]

sample_se <- sqrt(sample_pro * (1 - sample_pro) / n)

#90% Confidence Level
lower_vector <- sample_pro - 1.64* sample_se / sqrt(n)
upper_vector <- sample_pro + 1.64 * sample_se / sqrt(n)
ci <- c(lower_vector, upper_vector)
c("90% CI", paste(round(ci,3)))

#95% Confidence Level
lower_vector <- sample_pro - 1.96 * sample_se / sqrt(n)
upper_vector <- sample_pro + 1.96 * sample_se / sqrt(n)
ci <- c(lower_vector, upper_vector)
c("95% CI", paste(round(ci,3)))

#99% Confidence Level
lower_vector <- sample_pro - 2.58 * sample_se / sqrt(n)
upper_vector <- sample_pro + 2.58 * sample_se / sqrt(n)
ci <- c(lower_vector, upper_vector)
c("99% CI", paste(round(ci,3)))
