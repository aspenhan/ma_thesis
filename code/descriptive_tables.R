# Sample descriptive statistics: sample sizes, covariate balance tables and joint balance tests,
# slider task performance summary tables, and tests of performance differences between subsamples
# Outputs: output/tables/covariates_sumstats_*.tex, output/tables/performance_sumstats_*.tex,
#          output/numbers/descriptive_numbers.tex (numbers cited in the text),
#          results/numbers_in_paper/balance_tests.csv, results/numbers_in_paper/subsample_performance_tests.csv

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

sample_sizes <- list(NumObs = nrow(slider_data),
                     NumObsLab = nrow(slider_lab_data),
                     NumObsClass = nrow(slider_data %>% filter(sample != "lab")),
                     NumObsMicro = nrow(slider_micro_data),
                     NumObsMacro = nrow(slider_macro_data),
                     NumRespondents = nrow(slider_data_full))

# ----- Covariate balance tables -----

## ----- Lab sample -----

# Fisher's exact test version

set.seed(1)

lab_covariate_sumstats <- slider_lab_data %>%
  select(treatment, gender, race, age, income, edu, econ, occupation, mouse) %>%
  rename("household income" = income, "education level" = edu,
         "studied economics" = econ, "used mouse" = mouse) %>%
  rename_with(str_to_title) %>%
  tbl_summary(
    missing = "no",
    by = Treatment,
    type = where(is.numeric) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)")
      ) %>%
      modify_header(label ~ "**Metric**", stat_1 ~ "**1**", stat_2 ~ "**2**", stat_3 ~ "**3**", stat_4 ~ "**4**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Treatments**") %>%
  add_overall(col_label = "**All**") %>%
  bold_labels() %>%
  add_p(list(all_continuous() ~ "kruskal.test", all_categorical() ~ "fisher.test"),
        test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)) %>%
  add_significance_stars()

# Chi square test version

lab_covariate_sumstats_alt <- slider_lab_data %>%
  select(treatment, gender, race, age, income, edu, econ, occupation, mouse) %>%
  rename("household income" = income, "education level" = edu,
         "studied economics" = econ, "used mouse" = mouse) %>%
  rename_with(str_to_title) %>%
  tbl_summary(
    missing = "no",
    by = Treatment,
    type = where(is.numeric) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)")
      ) %>%
  add_p(list(all_continuous() ~ "oneway.test", all_categorical() ~ "chisq.test")) %>%
  add_significance_stars() %>%
  modify_column_hide(columns = c("label", "stat_1", "stat_2", "stat_3", "stat_4"))

# Combine the tables

lab_covariate_sumstats <- tbl_merge(list(lab_covariate_sumstats, lab_covariate_sumstats_alt), tab_spanner = FALSE) %>%
  as_gt() %>%
  cols_width("label" ~ px(175), starts_with("p") ~ px(50)) %>%
  tab_options(table.font.size = 12)

save_gt_tex(lab_covariate_sumstats, "output/tables/covariates_sumstats_lab.tex")

## ----- Micro sample -----

# Fisher's exact test version

set.seed(2)

micro_covariate_sumstats <- slider_micro_data %>%
  select(treatment, gender, race, age, income, mouse) %>%
  rename("household income" = income, "used mouse" = mouse) %>%
  rename_with(str_to_title) %>%
  tbl_summary(
    missing = "no",
    by = Treatment,
    type = where(is.numeric) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)")
      ) %>%
      modify_header(label ~ "**Metric**", stat_1 ~ "**1**", stat_2 ~ "**2**", stat_3 ~ "**3**", stat_4 ~ "**4**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Treatments**") %>%
  add_overall(col_label = "**All**") %>%
  bold_labels() %>%
  add_p(list(all_continuous() ~ "kruskal.test", all_categorical() ~ "fisher.test"),
        test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)) %>%
  add_significance_stars()

# Chi square test version

micro_covariate_sumstats_alt <- slider_micro_data %>%
  select(treatment, gender, race, age, income, mouse) %>%
  rename("household income" = income, "used mouse" = mouse) %>%
  rename_with(str_to_title) %>%
  tbl_summary(
    missing = "no",
    by = Treatment,
    type = where(is.numeric) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)")
      ) %>%
  add_p(list(all_continuous() ~ "oneway.test", all_categorical() ~ "chisq.test")) %>%
  add_significance_stars() %>%
  modify_column_hide(columns = c("label", "stat_1", "stat_2", "stat_3", "stat_4"))

# Combine the tables

micro_covariate_sumstats <- tbl_merge(list(micro_covariate_sumstats, micro_covariate_sumstats_alt), tab_spanner = FALSE) %>%
  as_gt() %>%
  cols_width("label" ~ px(175), starts_with("stat") ~ px(75),
             starts_with("p") ~ px(50)) %>%
  tab_options(table.font.size = 12)

save_gt_tex(micro_covariate_sumstats, "output/tables/covariates_sumstats_micro.tex")

## ----- Macro sample -----

# Fisher's exact test version

set.seed(3)

macro_covariate_sumstats <- slider_macro_data %>%
  select(treatment, gender, race, age, income, mouse) %>%
  rename("household income" = income, "used mouse" = mouse) %>%
  rename_with(str_to_title) %>%
  tbl_summary(
    missing = "no",
    by = Treatment,
    type = where(is.numeric) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)")
      ) %>%
      modify_header(label ~ "**Metric**", stat_1 ~ "**1**", stat_2 ~ "**2**", stat_3 ~ "**3**", stat_4 ~ "**4**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Treatments**") %>%
  add_overall(col_label = "**All**") %>%
  bold_labels() %>%
  add_p(list(all_continuous() ~ "kruskal.test", all_categorical() ~ "fisher.test"),
        test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)) %>%
  add_significance_stars()

# Chi square test version

macro_covariate_sumstats_alt <- slider_macro_data %>%
  select(treatment, gender, race, age, income, mouse) %>%
  rename("household income" = income, "used mouse" = mouse) %>%
  rename_with(str_to_title) %>%
  tbl_summary(
    missing = "no",
    by = Treatment,
    type = where(is.numeric) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)")
      ) %>%
  add_p(list(all_continuous() ~ "oneway.test", all_categorical() ~ "chisq.test")) %>%
  add_significance_stars() %>%
  modify_column_hide(columns = c("label", "stat_1", "stat_2", "stat_3", "stat_4"))

# Combine the tables

macro_covariate_sumstats <- tbl_merge(list(macro_covariate_sumstats, macro_covariate_sumstats_alt), tab_spanner = FALSE) %>%
  as_gt() %>%
  cols_width("label" ~ px(175), starts_with("stat") ~ px(75),
             starts_with("p") ~ px(50)) %>%
  tab_options(table.font.size = 12)

save_gt_tex(macro_covariate_sumstats, "output/tables/covariates_sumstats_macro.tex")

## ----- Pooled sample -----

# Fisher's exact test version

set.seed(4)

pooled_covariate_sumstats <- slider_data %>%
  select(treatment, gender, race, age, income, edu, econ, occupation, mouse) %>%
  rename("household income" = income, "education level" = edu,
         "studied economics" = econ, "used mouse" = mouse) %>%
  rename_with(str_to_title) %>%
  tbl_summary(
    missing = "no",
    by = Treatment,
    type = where(is.numeric) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)")
      ) %>%
      modify_header(label ~ "**Metric**", stat_1 ~ "**1**", stat_2 ~ "**2**", stat_3 ~ "**3**", stat_4 ~ "**4**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Treatments**") %>%
  add_overall(col_label = "**All**") %>%
  bold_labels() %>%
  add_p(list(all_continuous() ~ "kruskal.test", all_categorical() ~ "fisher.test"),
        test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)) %>%
  add_significance_stars()

# Chi square test version

pooled_covariate_sumstats_alt <- slider_data %>%
  select(treatment, gender, race, age, income, edu, econ, occupation, mouse) %>%
  rename("household income" = income, "education level" = edu,
         "studied economics" = econ, "used mouse" = mouse) %>%
  rename_with(str_to_title) %>%
  tbl_summary(
    missing = "no",
    by = Treatment,
    type = where(is.numeric) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)")
      ) %>%
  add_p(list(all_continuous() ~ "oneway.test", all_categorical() ~ "chisq.test")) %>%
  add_significance_stars() %>%
  modify_column_hide(columns = c("label", "stat_1", "stat_2", "stat_3", "stat_4"))

# Combine the tables

pooled_covariate_sumstats <-
  tbl_merge(list(pooled_covariate_sumstats, pooled_covariate_sumstats_alt), tab_spanner = FALSE) %>%
  as_gt() %>%
  cols_width("label" ~ px(175), starts_with("p") ~ px(50)) %>%
  tab_options(table.font.size = 12)

save_gt_tex(pooled_covariate_sumstats, "output/tables/covariates_sumstats_pooled.tex")

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

write_csv(balance_tests, "results/numbers_in_paper/balance_tests.csv")

# ----- Slider task performance summary tables -----

## ----- Lab sample -----

lab_slider_performance_sumstats <- slider_lab_data %>%
  select(treatment, sets_attempted, sets_completed, mistake_rate) %>%
    tbl_summary(
        by = treatment,
        missing = "no",
        type = where(is.numeric) ~ "continuous",
        statistic = list(all_continuous() ~ "{mean} ({sd})",
                         all_categorical() ~ "{n} ({p}%)"),
        label = c(sets_attempted ~ "Sets attempted",
                  sets_completed ~ "Sets completed",
                  mistake_rate ~ "Mistake rate (%)")
      ) %>%
  add_overall(col_label = "**All**") %>%
      modify_header(label ~ "**Metric**", stat_1 ~ "**Treatment 1**", stat_2 ~ "**Treatment 2**", stat_3 ~ "**Treatment 3**", stat_4 ~ "**Treatment 4**") %>%
  modify_spanning_header(all_stat_cols() ~ "**RFCDR Lab**")

save_gt_tex(as_gt(lab_slider_performance_sumstats), "output/tables/performance_sumstats_lab.tex")

## ----- Microecon class sample -----

micro_slider_performance_sumstats <- slider_micro_data %>%
  select(treatment, sets_attempted, sets_completed, mistake_rate) %>%
    tbl_summary(
        by = treatment,
        type = where(is.numeric) ~ "continuous",
        statistic = list(all_continuous() ~ "{mean} ({sd})",
                         all_categorical() ~ "{n} ({p}%)"),
        label = c(sets_attempted ~ "Sets attempted",
                  sets_completed ~ "Sets completed",
                  mistake_rate ~ "Mistake rate (%)")
      ) %>%
  add_overall(col_label = "**All**") %>%
      modify_header(label ~ "**Metric**", stat_1 ~ "**Treatment 1**", stat_2 ~ "**Treatment 2**", stat_3 ~ "**Treatment 3**", stat_4 ~ "**Treatment 4**") %>%
    modify_spanning_header(all_stat_cols() ~ "**Microeconomics Class**")

save_gt_tex(as_gt(micro_slider_performance_sumstats), "output/tables/performance_sumstats_micro.tex")

## ----- Macroecon class sample -----

macro_slider_performance_sumstats <- slider_macro_data %>%
  select(treatment, sets_attempted, sets_completed, mistake_rate) %>%
    tbl_summary(
        by = treatment,
        type = where(is.numeric) ~ "continuous",
        statistic = list(all_continuous() ~ "{mean} ({sd})",
                         all_categorical() ~ "{n} ({p}%)"),
        label = c(sets_attempted ~ "Sets attempted",
                  sets_completed ~ "Sets completed",
                  mistake_rate ~ "Mistake rate (%)")
      ) %>%
  add_overall(col_label = "**All**") %>%
      modify_header(label ~ "**Metric**", stat_1 ~ "**Treatment 1**", stat_2 ~ "**Treatment 2**", stat_3 ~ "**Treatment 3**", stat_4 ~ "**Treatment 4**") %>%
    modify_spanning_header(all_stat_cols() ~ "**Macroeconomics Class**")

save_gt_tex(as_gt(macro_slider_performance_sumstats), "output/tables/performance_sumstats_macro.tex")

## ----- Pooled sample -----

slider_performance_sumstats <- slider_data %>%
  select(treatment, sets_attempted, sets_completed, mistake_rate) %>%
    tbl_summary(
        by = treatment,
        type = where(is.numeric) ~ "continuous",
        statistic = list(all_continuous() ~ "{mean} ({sd})",
                         all_categorical() ~ "{n} ({p}%)"),
        label = c(sets_attempted ~ "Sets attempted",
                  sets_completed ~ "Sets completed",
                  mistake_rate ~ "Mistake rate (%)")
      ) %>%
  add_overall(col_label = "**All**") %>%
      modify_header(label ~ "**Metric**", stat_1 ~ "**Treatment 1**", stat_2 ~ "**Treatment 2**", stat_3 ~ "**Treatment 3**", stat_4 ~ "**Treatment 4**") %>%
    modify_spanning_header(all_stat_cols() ~ "**Pooled**")

save_gt_tex(as_gt(slider_performance_sumstats), "output/tables/performance_sumstats_pooled.tex")

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

write_csv(subsample_performance_tests, "results/numbers_in_paper/subsample_performance_tests.csv")

# ----- Numbers cited in the text -----

# Joint balance tests for each sample (e.g. BalanceLabChisq and BalanceLabPval)

balance_test_macros <- c(
  set_names(as.list(round(balance_tests$chisq, 2)), str_c("Balance", str_to_title(balance_tests$sample), "Chisq")),
  set_names(as.list(format_p_value(balance_tests$p.value)), str_c("Balance", str_to_title(balance_tests$sample), "Pval")))

write_tex_macros(c(sample_sizes, balance_test_macros),
                 "output/numbers/descriptive_numbers.tex")
