# Distributional comparisons of slider task performance between treatments (pooled sample):
# empirical CDFs, joint Kruskal-Wallis tests, and pair-wise Kolmogorov-Smirnov tests
# Outputs: output/figures/dist_comp_*_pooled.pdf, output/tests/distribution_tests_pooled.csv
# Lab and class sample counterparts are in code/appendix/distribution_test_appendix.R

# ----- Setup -----

# Load packages and helpers, unless already done by code/run_all.R

if (!exists("run_all")) source("code/setup.R")

# ----- Load data -----

slider_data <- read_rds("data/slider_full_data_clean.rds")

# ----- Distributions of key performance metrics across treatments -----

## ----- Sets completed -----

sets_completed_dist_comp <- ggplot(data = slider_data, aes(x = sets_completed, color = treatment)) +
  stat_ecdf(geom = "step", linewidth = 0.8) +
  labs(x = "Sets completed", y = "Probability", color = "Treatments") +
  def_theme

save_figure_pdf(sets_completed_dist_comp, "output/figures/dist_comp_sets_completed_pooled.pdf")

## ----- Mistake rates -----

mistake_rate_dist_comp <- ggplot(data = slider_data, aes(x = mistake_rate, color = treatment)) +
  stat_ecdf(geom = "step", linewidth = 0.8) +
  labs(x = "Mistake rate", y = "Probability", color = "Treatments") +
  def_theme

save_figure_pdf(mistake_rate_dist_comp, "output/figures/dist_comp_mistake_rate_pooled.pdf")

# ----- Tests of differences between treatments -----

## ----- Subset data by treatment for pair-wise tests -----

slider_data_t1 <- slider_data %>%
  filter(treatment == 1)

slider_data_t2 <- slider_data %>%
  filter(treatment == 2)

slider_data_t3 <- slider_data %>%
  filter(treatment == 3)

slider_data_t4 <- slider_data %>%
  filter(treatment == 4)

## ----- Joint and pair-wise tests -----

distribution_tests_pooled <- list(

  # Joint tests

  "Sets completed: all treatments" = kruskal.test(sets_completed ~ treatment, data = slider_data),
  "Mistake rate: all treatments" = kruskal.test(mistake_rate ~ treatment, data = slider_data), #sig diff

  # Pair-wise tests for sets completed (no sig diff for all)

  "Sets completed: 1 vs 2" = ks.test(slider_data_t1$sets_completed, slider_data_t2$sets_completed),
  "Sets completed: 1 vs 3" = ks.test(slider_data_t1$sets_completed, slider_data_t3$sets_completed),
  "Sets completed: 1 vs 4" = ks.test(slider_data_t1$sets_completed, slider_data_t4$sets_completed),
  "Sets completed: 2 vs 3" = ks.test(slider_data_t2$sets_completed, slider_data_t3$sets_completed),
  "Sets completed: 2 vs 4" = ks.test(slider_data_t2$sets_completed, slider_data_t4$sets_completed),
  "Sets completed: 3 vs 4" = ks.test(slider_data_t3$sets_completed, slider_data_t4$sets_completed),

  # Pair-wise tests for mistake rate

  "Mistake rate: 1 vs 2" = ks.test(slider_data_t1$mistake_rate, slider_data_t2$mistake_rate),
  "Mistake rate: 1 vs 3" = ks.test(slider_data_t1$mistake_rate, slider_data_t3$mistake_rate), #sig diff
  "Mistake rate: 1 vs 4" = ks.test(slider_data_t1$mistake_rate, slider_data_t4$mistake_rate),
  "Mistake rate: 2 vs 3" = ks.test(slider_data_t2$mistake_rate, slider_data_t3$mistake_rate), #sig diff
  "Mistake rate: 2 vs 4" = ks.test(slider_data_t2$mistake_rate, slider_data_t4$mistake_rate),
  "Mistake rate: 3 vs 4" = ks.test(slider_data_t3$mistake_rate, slider_data_t4$mistake_rate) #sig diff

  ) %>%
  tidy_tests()

distribution_tests_pooled

write_csv(distribution_tests_pooled, "output/tests/distribution_tests_pooled.csv")
