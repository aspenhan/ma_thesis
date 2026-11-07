# Sample descriptive statistics: sample sizes, covariate balance tables and joint balance tests,
# slider task performance summary tables, and tests of performance differences between subsamples
# Outputs: output/tables/covariates_sumstats_*.tex, output/tables/performance_sumstats*.tex,
#          results/numbers_in_paper/sample_sizes.csv, results/numbers_in_paper/balance_tests.csv,
#          results/numbers_in_paper/subsample_performance_tests.csv

# ----- Setup -----

# Load packages and helpers, unless already done by code/run_all.R

if (!exists("run_all")) source("code/setup.R")

# ----- Load data -----

slider_data <- read_rds("data/slider_full_data_clean.rds")

slider_lab_data <- read_rds("data/slider_lab_data_clean.rds")

slider_micro_data <- read_rds("data/slider_micro_data_clean.rds")

slider_macro_data <- read_rds("data/slider_macro_data_clean.rds")

slider_data_full <- read_rds("data/slider_all_respondents_data.rds") #including those who did not finish

# ----- Sample sizes -----

sample_sizes <- tibble(sample = c("pooled", "lab", "class", "micro", "macro", "all respondents"),
                       n = c(nrow(slider_data), nrow(slider_lab_data), nrow(slider_data %>% filter(sample != "lab")),
                             nrow(slider_micro_data), nrow(slider_macro_data), nrow(slider_data_full)))

print(sample_sizes)

write_with_retry(function() write_csv(sample_sizes, "results/numbers_in_paper/sample_sizes.csv"))

# ----- Covariate balance tables -----

## ----- Table helpers -----

# Covariates in each sample (education, economics studies and occupation are constant in the class samples)

covariates_all <- c("gender", "race", "age", "income", "edu", "econ", "occupation", "mouse")

covariates_class <- c("gender", "race", "age", "income", "mouse")

# Summary of covariates by treatment group: proportions for categories, the mean for age

covariate_summary <- function(data, vars) {
  data %>%
    relabel_covariates() %>%
    select(treatment, all_of(vars)) %>%
    tbl_summary(
      by = treatment,
      missing = "no",
      type = where(is.numeric) ~ "continuous",
      label = as.list(covariate_names[vars]),
      statistic = list(all_continuous() ~ "{mean}",
                       all_categorical() ~ "{p}"),
      digits = list(all_continuous() ~ 1,
                    all_categorical() ~ list(p = label_style_number(digits = 2))))
}

# Balance table: covariate summaries for all participants and each group, with p-values of tests of differences between
# groups from (a) Fisher's exact (simulated) and Kruskal-Wallis tests and (b) chi-square and Welch's one-way F tests

covariate_table <- function(data, vars, seed) {

  # Fisher's exact test version (simulated p-values, so the seed is set first)

  set.seed(seed)

  exact <- covariate_summary(data, vars) %>%
    add_overall(last = FALSE) %>%
    add_p(list(all_continuous() ~ "kruskal.test", all_categorical() ~ "fisher.test"),
          test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000),
          pvalue_fun = label_style_pvalue(digits = 2)) %>%
    add_significance_stars(thresholds = star_levels)

  # Chi square test version

  asymptotic <- covariate_summary(data, vars) %>%
    add_p(list(all_continuous() ~ "oneway.test", all_categorical() ~ "chisq.test"),
          pvalue_fun = label_style_pvalue(digits = 2)) %>%
    add_significance_stars(thresholds = star_levels) %>%
    modify_column_hide(columns = c(label, starts_with("stat_")))

  # Combine the tables

  tbl_merge(list(exact, asymptotic), tab_spanner = FALSE) %>%
    add_n_row(data, suffix = "_1") %>%
    modify_header(label = "", stat_0_1 = "All",
                  stat_1_1 = treatment_names[["1"]], stat_2_1 = treatment_names[["2"]],
                  stat_3_1 = treatment_names[["3"]], stat_4_1 = treatment_names[["4"]],
                  p.value_1 = "(a)", p.value_2 = "(b)") %>%
    modify_spanning_header(c(stat_1_1, stat_2_1, stat_3_1, stat_4_1) ~ "Treatment group",
                           c(p.value_1, p.value_2) ~ "p-value") %>%
    finish_gt() %>%
    cols_width(label ~ px(170), starts_with("p.value") ~ px(56)) %>%
    tab_options(table.font.size = 12)
}

## ----- Lab sample -----

save_gt_tex(covariate_table(slider_lab_data, covariates_all, seed = 1), "output/tables/covariates_sumstats_lab.tex")

## ----- Micro sample -----

save_gt_tex(covariate_table(slider_micro_data, covariates_class, seed = 2), "output/tables/covariates_sumstats_micro.tex")

## ----- Macro sample -----

save_gt_tex(covariate_table(slider_macro_data, covariates_class, seed = 3), "output/tables/covariates_sumstats_macro.tex")

## ----- Pooled sample -----

save_gt_tex(covariate_table(slider_data, covariates_all, seed = 4), "output/tables/covariates_sumstats_pooled.tex")

# ----- Joint tests of covariate balance -----

# Likelihood ratio test of multinomial logit on covariates vs intercept only

## ----- Lab sample -----

reg_balance_lab <- multinom(treatment ~ gender + race + age + income + edu + econ + occupation + mouse, data = slider_lab_data, trace = FALSE)

reg_balance_lab_null <- slider_lab_data %>%
    select(treatment, gender, race, age, income, edu, econ, occupation, mouse) %>%
  na.omit() %>%
  multinom(treatment ~ 1, data = ., trace = FALSE)

balance_test_lab <- lmtest::lrtest(reg_balance_lab_null, reg_balance_lab)

## ----- Micro sample -----

reg_balance_micro <- multinom(treatment ~ gender + race + age + income + mouse, data = slider_micro_data, trace = FALSE)

reg_balance_micro_null <- slider_micro_data %>%
    select(treatment, gender, race, age, income, mouse) %>%
  na.omit() %>%
  multinom(treatment ~ 1, data = ., trace = FALSE)

balance_test_micro <- lmtest::lrtest(reg_balance_micro_null, reg_balance_micro)

## ----- Macro sample -----

reg_balance_macro <- multinom(treatment ~ gender + race + age + income + mouse, data = slider_macro_data, trace = FALSE)

reg_balance_macro_null <- slider_macro_data %>%
    select(treatment, gender, race, age, income, mouse) %>%
  na.omit() %>%
  multinom(treatment ~ 1, data = ., trace = FALSE)

balance_test_macro <- lmtest::lrtest(reg_balance_macro_null, reg_balance_macro)

## ----- Pooled sample -----

reg_balance_pooled <- multinom(treatment ~ gender + race + age + income + edu + econ + occupation + mouse, data = slider_data, trace = FALSE)

reg_balance_pooled_null <- slider_data %>%
    select(treatment, gender, race, age, income, edu, econ, occupation, mouse) %>%
  na.omit() %>%
  multinom(treatment ~ 1, data = ., trace = FALSE)

balance_test_pooled <- lmtest::lrtest(reg_balance_pooled_null, reg_balance_pooled)

## ----- Collect joint test results -----

balance_tests <- list(lab = balance_test_lab, micro = balance_test_micro,
                      macro = balance_test_macro, pooled = balance_test_pooled) %>%
  map(~ tibble(chisq = .x$Chisq[2], df = .x$Df[2], p.value = .x$"Pr(>Chisq)"[2])) %>%
  bind_rows(.id = "sample")

print(balance_tests)

write_with_retry(function() write_csv(balance_tests, "results/numbers_in_paper/balance_tests.csv"))

# ----- Slider task performance summary tables -----

## ----- Table helpers -----

# Mean (SD) of slider task performance for all participants and each treatment group, with the number of participants

performance_summary <- function(data, sample_name = NULL) {
  tbl <- data %>%
    select(treatment, sets_attempted, sets_completed, mistake_rate) %>%
    tbl_summary(
      by = treatment,
      missing = "no",
      type = where(is.numeric) ~ "continuous",
      statistic = all_continuous() ~ "{mean} ({sd})",
      digits = all_continuous() ~ 1,
      label = list(sets_attempted ~ "Sets attempted",
                   sets_completed ~ "Sets completed",
                   mistake_rate ~ "Mistake rate (%)")) %>%
    add_overall(last = FALSE) %>%
    add_n_row(data) %>%
    modify_header(label = "", stat_0 = "All",
                  stat_1 = treatment_names[["1"]], stat_2 = treatment_names[["2"]],
                  stat_3 = treatment_names[["3"]], stat_4 = treatment_names[["4"]])
  if (!is.null(sample_name)) tbl <- modify_spanning_header(tbl, all_stat_cols() ~ sample_name)
  tbl
}

save_performance_tex <- function(tbl, file) {
  tbl %>%
    finish_gt() %>%
    cols_width(label ~ px(120)) %>%
    tab_options(table.font.size = 14) %>%
    save_gt_tex(file)
}

## ----- Each sample (stacked in one float in the manuscripts) -----

save_performance_tex(performance_summary(slider_data, "Pooled"), "output/tables/performance_sumstats_pooled.tex")

save_performance_tex(performance_summary(slider_lab_data, "Lab"), "output/tables/performance_sumstats_lab.tex")

save_performance_tex(performance_summary(slider_micro_data, "Microeconomics class"), "output/tables/performance_sumstats_micro.tex")

save_performance_tex(performance_summary(slider_macro_data, "Macroeconomics class"), "output/tables/performance_sumstats_macro.tex")

## ----- All samples in one table, with a panel for each sample -----

tbl_stack(list(performance_summary(slider_data), performance_summary(slider_lab_data),
               performance_summary(slider_micro_data), performance_summary(slider_macro_data)),
          group_header = c("Pooled", "Lab", "Microeconomics class", "Macroeconomics class")) %>%
  save_performance_tex("output/tables/performance_sumstats.tex")

# ----- Tests of performance differences between subsamples -----

subsample_performance_tests <- list(

  # Joint tests

  "Sets attempted: F test" = oneway.test(sets_attempted ~ sample, data = slider_data), #sig diff
  "Sets attempted: Shapiro-Wilk" = shapiro.test(slider_data$sets_attempted), #reject that data is normal so F test is less suitable
  "Sets attempted: Kruskal-Wallis" = kruskal.test(sets_attempted ~ sample, data = slider_data), #sig diff

  "Sets completed: F test" = oneway.test(sets_completed ~ sample, data = slider_data), #sig diff
  "Sets completed: Shapiro-Wilk" = shapiro.test(slider_data$sets_completed), #reject that data is normal so F test is less suitable
  "Sets completed: Kruskal-Wallis" = kruskal.test(sets_completed ~ sample, data = slider_data), #sig diff

  "Mistake rate: F test" = oneway.test(mistake_rate ~ sample, data = slider_data), #no sig diff
  "Mistake rate: Shapiro-Wilk" = shapiro.test(slider_data$mistake_rate), #reject that data is normal so F test is not suitable
  "Mistake rate: Kruskal-Wallis" = kruskal.test(mistake_rate ~ sample, data = slider_data), #no sig diff

  # Pairwise t tests

  "Sets attempted: micro vs macro" = t.test(slider_micro_data$sets_attempted, slider_macro_data$sets_attempted), #no sig diff
  "Sets attempted: micro vs lab" = t.test(slider_micro_data$sets_attempted, slider_lab_data$sets_attempted), #sig diff
  "Sets attempted: macro vs lab" = t.test(slider_macro_data$sets_attempted, slider_lab_data$sets_attempted), #sig diff

  "Sets completed: micro vs macro" = t.test(slider_micro_data$sets_completed, slider_macro_data$sets_completed), #no sig diff
  "Sets completed: micro vs lab" = t.test(slider_micro_data$sets_completed, slider_lab_data$sets_completed), #sig diff
  "Sets completed: macro vs lab" = t.test(slider_macro_data$sets_completed, slider_lab_data$sets_completed) #sig diff

  ) %>%
  tidy_tests()

print(subsample_performance_tests, n = Inf)

write_with_retry(function() write_csv(subsample_performance_tests, "results/numbers_in_paper/subsample_performance_tests.csv"))
