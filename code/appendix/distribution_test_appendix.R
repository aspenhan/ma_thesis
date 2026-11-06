# Appendix A1: distributional comparisons of slider task performance between treatments for the
# lab and class samples (counterparts of code/distribution_test.R for the pooled sample)
# Outputs: output/figures/dist_comp_*_lab.pdf, output/figures/dist_comp_*_class.pdf,
#          output/numbers/distribution_numbers_appendix.tex (p-values cited in the text),
#          results/numbers_in_paper/distribution_tests_lab.csv, results/numbers_in_paper/distribution_tests_class.csv

# ----- Setup -----

# Load packages and helpers, unless already done by code/run_all.R

if (!exists("run_all")) source("code/setup.R")

# ----- Load data -----

slider_lab_data <- read_rds("data/slider_lab_data_clean.rds")

slider_class_data <- read_rds("data/slider_class_data_clean.rds")

# ----- Lab sample -----

## ----- Sets completed distribution -----

sets_completed_dist_comp_lab <- ggplot(data = slider_lab_data, aes(x = sets_completed, color = treatment)) +
  stat_ecdf(geom = "step", linewidth = 0.8) +
  labs(x = "Sets completed", y = "Cumulative proportion", color = "Treatment group") +
  scale_color_discrete(labels = treatment_names) +
  def_theme

save_figure_pdf(sets_completed_dist_comp_lab, "output/figures/dist_comp_sets_completed_lab.pdf")

## ----- Mistake rate distribution -----

mistake_rate_dist_comp_lab <- ggplot(data = slider_lab_data, aes(x = mistake_rate, color = treatment)) +
  stat_ecdf(geom = "step", linewidth = 0.8) +
  labs(x = "Mistake rate (%)", y = "Cumulative proportion", color = "Treatment group") +
  scale_color_discrete(labels = treatment_names) +
  def_theme

save_figure_pdf(mistake_rate_dist_comp_lab, "output/figures/dist_comp_mistake_rate_lab.pdf")

## ----- Subset data by treatment for pair-wise tests -----

slider_lab_data_t1 <- slider_lab_data %>%
  filter(treatment == 1)

slider_lab_data_t2 <- slider_lab_data %>%
  filter(treatment == 2)

slider_lab_data_t3 <- slider_lab_data %>%
  filter(treatment == 3)

slider_lab_data_t4 <- slider_lab_data %>%
  filter(treatment == 4)

## ----- Tests of differences between treatments -----

# No sig diff after correcting for multiple hypotheses

distribution_tests_lab <- list(

  # Joint tests

  "Sets completed: all treatments" = kruskal.test(sets_completed ~ treatment, data = slider_lab_data),
  "Mistake rate: all treatments" = kruskal.test(mistake_rate ~ treatment, data = slider_lab_data),

  # Pair-wise tests for sets completed

  "Sets completed: 1 vs 2" = ks.test(slider_lab_data_t1$sets_completed, slider_lab_data_t2$sets_completed),
  "Sets completed: 1 vs 3" = ks.test(slider_lab_data_t1$sets_completed, slider_lab_data_t3$sets_completed),
  "Sets completed: 1 vs 4" = ks.test(slider_lab_data_t1$sets_completed, slider_lab_data_t4$sets_completed),
  "Sets completed: 2 vs 3" = ks.test(slider_lab_data_t2$sets_completed, slider_lab_data_t3$sets_completed),
  "Sets completed: 2 vs 4" = ks.test(slider_lab_data_t2$sets_completed, slider_lab_data_t4$sets_completed),
  "Sets completed: 3 vs 4" = ks.test(slider_lab_data_t3$sets_completed, slider_lab_data_t4$sets_completed),

  # Pair-wise tests for mistake rate

  "Mistake rate: 1 vs 2" = ks.test(slider_lab_data_t1$mistake_rate, slider_lab_data_t2$mistake_rate),
  "Mistake rate: 1 vs 3" = ks.test(slider_lab_data_t1$mistake_rate, slider_lab_data_t3$mistake_rate),
  "Mistake rate: 1 vs 4" = ks.test(slider_lab_data_t1$mistake_rate, slider_lab_data_t4$mistake_rate),
  "Mistake rate: 2 vs 3" = ks.test(slider_lab_data_t2$mistake_rate, slider_lab_data_t3$mistake_rate),
  "Mistake rate: 2 vs 4" = ks.test(slider_lab_data_t2$mistake_rate, slider_lab_data_t4$mistake_rate),
  "Mistake rate: 3 vs 4" = ks.test(slider_lab_data_t3$mistake_rate, slider_lab_data_t4$mistake_rate)

  ) %>%
  tidy_tests()

print(distribution_tests_lab, n = Inf)

write_with_retry(function() write_csv(distribution_tests_lab, "results/numbers_in_paper/distribution_tests_lab.csv"))

# ----- Class sample -----

## ----- Sets completed distribution -----

sets_completed_dist_comp_class <- ggplot(data = slider_class_data, aes(x = sets_completed, color = treatment)) +
  stat_ecdf(geom = "step", linewidth = 0.8) +
  labs(x = "Sets completed", y = "Cumulative proportion", color = "Treatment group") +
  scale_color_discrete(labels = treatment_names) +
  def_theme

save_figure_pdf(sets_completed_dist_comp_class, "output/figures/dist_comp_sets_completed_class.pdf")

## ----- Mistake rate distribution -----

mistake_rate_dist_comp_class <- ggplot(data = slider_class_data, aes(x = mistake_rate, color = treatment)) +
  stat_ecdf(geom = "step", linewidth = 0.8) +
  labs(x = "Mistake rate (%)", y = "Cumulative proportion", color = "Treatment group") +
  scale_color_discrete(labels = treatment_names) +
  def_theme

save_figure_pdf(mistake_rate_dist_comp_class, "output/figures/dist_comp_mistake_rate_class.pdf")

## ----- Subset data by treatment for pair-wise tests -----

slider_class_data_t1 <- slider_class_data %>%
  filter(treatment == 1)

slider_class_data_t2 <- slider_class_data %>%
  filter(treatment == 2)

slider_class_data_t3 <- slider_class_data %>%
  filter(treatment == 3)

slider_class_data_t4 <- slider_class_data %>%
  filter(treatment == 4)

## ----- Tests of differences between treatments -----

distribution_tests_class <- list(

  # Joint tests

  "Sets completed: all treatments" = kruskal.test(sets_completed ~ treatment, data = slider_class_data),
  "Mistake rate: all treatments" = kruskal.test(mistake_rate ~ treatment, data = slider_class_data), #sig diff

  # Pair-wise tests for sets completed

  "Sets completed: 1 vs 2" = ks.test(slider_class_data_t1$sets_completed, slider_class_data_t2$sets_completed),
  "Sets completed: 1 vs 3" = ks.test(slider_class_data_t1$sets_completed, slider_class_data_t3$sets_completed),
  "Sets completed: 1 vs 4" = ks.test(slider_class_data_t1$sets_completed, slider_class_data_t4$sets_completed),
  "Sets completed: 2 vs 3" = ks.test(slider_class_data_t2$sets_completed, slider_class_data_t3$sets_completed),
  "Sets completed: 2 vs 4" = ks.test(slider_class_data_t2$sets_completed, slider_class_data_t4$sets_completed),
  "Sets completed: 3 vs 4" = ks.test(slider_class_data_t3$sets_completed, slider_class_data_t4$sets_completed),

  # Pair-wise tests for mistake rate

  "Mistake rate: 1 vs 2" = ks.test(slider_class_data_t1$mistake_rate, slider_class_data_t2$mistake_rate),
  "Mistake rate: 1 vs 3" = ks.test(slider_class_data_t1$mistake_rate, slider_class_data_t3$mistake_rate), #sig diff
  "Mistake rate: 1 vs 4" = ks.test(slider_class_data_t1$mistake_rate, slider_class_data_t4$mistake_rate),
  "Mistake rate: 2 vs 3" = ks.test(slider_class_data_t2$mistake_rate, slider_class_data_t3$mistake_rate), #sig diff
  "Mistake rate: 2 vs 4" = ks.test(slider_class_data_t2$mistake_rate, slider_class_data_t4$mistake_rate),
  "Mistake rate: 3 vs 4" = ks.test(slider_class_data_t3$mistake_rate, slider_class_data_t4$mistake_rate) #sig diff

  ) %>%
  tidy_tests()

print(distribution_tests_class, n = Inf)

write_with_retry(function() write_csv(distribution_tests_class, "results/numbers_in_paper/distribution_tests_class.csv"))

# ----- Numbers cited in the text -----

# P-values of each test (e.g. DistTestLabSetsCompletedOneVsFourPval)

write_tex_macros(c(p_value_macros(distribution_tests_lab$test, distribution_tests_lab$p.value, "DistTestLab"),
                   p_value_macros(distribution_tests_class$test, distribution_tests_class$p.value, "DistTestClass")),
                 "output/numbers/distribution_numbers_appendix.tex")
