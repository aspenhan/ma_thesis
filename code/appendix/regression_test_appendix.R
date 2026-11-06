# Appendix A1: regression estimates of treatment effects on effort for the lab and class samples
# (counterparts of code/regression_test.R for the pooled sample), with tests of equality of treatment coefficients
# Outputs: output/tables/reg_*_lab.tex, output/tables/reg_*_class.tex,
#          output/numbers/regression_numbers_appendix.tex (p-values cited in the text),
#          results/numbers_in_paper/coefficient_tests_lab.csv, results/numbers_in_paper/coefficient_tests_class.csv

# ----- Setup -----

# Load packages and helpers, unless already done by code/run_all.R

if (!exists("run_all")) source("code/setup.R")

# ----- Load data -----

slider_lab_data <- read_rds("data/slider_lab_data_clean.rds")

slider_class_data <- read_rds("data/slider_class_data_clean.rds")

# ----- Lab sample -----

## ----- Prepare data -----

# Relevel education

slider_lab_data$edu <- relevel(slider_lab_data$edu, ref = "College degree or above")

# Create variable for target achievement

slider_lab_data <- slider_lab_data %>%
  mutate(accuracy_target = if_else(recorded_mistake_rate <= 10, "Yes", "No"),
         accuracy_target = factor(accuracy_target, levels = c("No", "Yes")))

# Table options: row labels, blocks of rows under a heading, and the covariates controlled for

lab_coef_labels <- regression_coef_map(slider_lab_data)

lab_groups <- regression_groups(slider_lab_data)

lab_covariates <- c("gender", "race", "income", "edu", "econ", "occupation", "mouse")

target_note <- "The outcome is meeting the accuracy target, that is, a recorded mistake rate of at most 10\\%."

## ----- Regression (1): accuracy target achievement -----

reg_target_lab_basic <- glm(accuracy_target ~ treatment + criterion, family = binomial, data = slider_lab_data)

reg_target_lab_baselines <- glm(accuracy_target ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, family = binomial, slider_lab_data)

reg_target_lab_covariates <- glm(accuracy_target ~ treatment + criterion + gender + race + income + edu + econ + occupation + mouse, family = binomial, slider_lab_data)

reg_target_lab_full <- glm(accuracy_target ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, family = binomial, slider_lab_data)

reg_target_lab_models <- list(reg_target_lab_basic, reg_target_lab_covariates, reg_target_lab_baselines, reg_target_lab_full)

save_regression_tex(reg_target_lab_models, "output/tables/reg_target_lab.tex",
                    outcome = "Log odds of meeting the accuracy target", coef_map = lab_coef_labels,
                    groups = lab_groups, note_size = "",
                    note = regression_note(reg_target_lab_models, slider_lab_data, "full", lab_covariates, target_note))

## ----- Regression (2): mistake rate -----

reg_mistakes_lab_basic <- lm_robust(mistake_rate ~ treatment + criterion, slider_lab_data)

reg_mistakes_lab_baselines <- lm_robust(mistake_rate ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_lab_data)

reg_mistakes_lab_covariates <- lm_robust(mistake_rate ~ treatment + criterion + gender + race + income + edu + econ + occupation + mouse, slider_lab_data)

reg_mistakes_lab_full <- lm_robust(mistake_rate ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, slider_lab_data)

reg_mistakes_lab_models <- list(reg_mistakes_lab_basic, reg_mistakes_lab_covariates, reg_mistakes_lab_baselines, reg_mistakes_lab_full)

save_regression_tex(reg_mistakes_lab_models, "output/tables/reg_mistakes_lab.tex",
                    outcome = "Mistake rate (\\%)", coef_map = lab_coef_labels,
                    groups = lab_groups, note_size = "",
                    note = regression_note(reg_mistakes_lab_models, slider_lab_data, "full", lab_covariates))

## ----- Regression (3): sets completed -----

reg_sets_lab_basic <- lm_robust(sets_completed ~ treatment + criterion, slider_lab_data)

reg_sets_lab_baselines <- lm_robust(sets_completed ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_lab_data)

reg_sets_lab_covariates <- lm_robust(sets_completed ~ treatment + criterion + gender + race + income + edu + econ + occupation + mouse, slider_lab_data)

reg_sets_lab_full <- lm_robust(sets_completed ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, slider_lab_data)

reg_sets_lab_models <- list(reg_sets_lab_basic, reg_sets_lab_covariates, reg_sets_lab_baselines, reg_sets_lab_full)

save_regression_tex(reg_sets_lab_models, "output/tables/reg_sets_lab.tex",
                    outcome = "Sets completed", coef_map = lab_coef_labels,
                    groups = lab_groups, note_size = "",
                    note = regression_note(reg_sets_lab_models, slider_lab_data, "full", lab_covariates))

## ----- Tests of equality of treatment coefficients -----

# Treatments 2 vs 3 (models 3 and 4)

coefficient_tests_lab <- tibble(
  outcome = rep(c("Accuracy target", "Mistake rate", "Sets completed"), each = 2),
  model = rep(c("Model 3", "Model 4"), times = 3),
  p.value = c(lh_p_value(reg_target_lab_baselines, "treatment2 = treatment3"),
              lh_p_value(reg_target_lab_full, "treatment2 = treatment3"),
              lh_p_value(reg_mistakes_lab_baselines, "treatment2 = treatment3"),
              lh_p_value(reg_mistakes_lab_full, "treatment2 = treatment3"),
              lh_p_value(reg_sets_lab_baselines, "treatment2 = treatment3"),
              lh_p_value(reg_sets_lab_full, "treatment2 = treatment3")))

print(coefficient_tests_lab)

write_csv(coefficient_tests_lab, "results/numbers_in_paper/coefficient_tests_lab.csv")

# ----- Class sample -----

# Education, economics studies and occupation are constant, so are not controlled for

## ----- Prepare data -----

# Relevel education

slider_class_data$edu <- relevel(slider_class_data$edu, ref = "College degree or above")

# Create variable for target achievement

slider_class_data <- slider_class_data %>%
  mutate(accuracy_target = if_else(recorded_mistake_rate <= 10, "Yes", "No"),
         accuracy_target = factor(accuracy_target, levels = c("No", "Yes")))

# Table options: row labels, blocks of rows under a heading, and the covariates controlled for

class_coef_labels <- regression_coef_map(slider_class_data)

class_groups <- regression_groups(slider_class_data)

class_covariates <- c("gender", "race", "income", "mouse")

## ----- Regression (1): accuracy target achievement -----

reg_target_class_basic <- glm(accuracy_target ~ treatment + criterion, family = binomial, data = slider_class_data)

reg_target_class_baselines <- glm(accuracy_target ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, family = binomial, slider_class_data)

reg_target_class_covariates <- glm(accuracy_target ~ treatment + criterion + gender + race + income + mouse, family = binomial, slider_class_data)

reg_target_class_full <- glm(accuracy_target ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + mouse, family = binomial, slider_class_data)

reg_target_class_models <- list(reg_target_class_basic, reg_target_class_covariates, reg_target_class_baselines, reg_target_class_full)

save_regression_tex(reg_target_class_models, "output/tables/reg_target_class.tex",
                    outcome = "Log odds of meeting the accuracy target", coef_map = class_coef_labels,
                    groups = class_groups, note_size = "",
                    note = regression_note(reg_target_class_models, slider_class_data, "full", class_covariates, target_note))

## ----- Regression (2): mistake rate -----

reg_mistakes_class_basic <- lm_robust(mistake_rate ~ treatment + criterion, slider_class_data)

reg_mistakes_class_baselines <- lm_robust(mistake_rate ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_class_data)

reg_mistakes_class_covariates <- lm_robust(mistake_rate ~ treatment + criterion + gender + race + income + mouse, slider_class_data)

reg_mistakes_class_full <- lm_robust(mistake_rate ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + mouse, slider_class_data)

reg_mistakes_class_models <- list(reg_mistakes_class_basic, reg_mistakes_class_covariates, reg_mistakes_class_baselines, reg_mistakes_class_full)

save_regression_tex(reg_mistakes_class_models, "output/tables/reg_mistakes_class.tex",
                    outcome = "Mistake rate (\\%)", coef_map = class_coef_labels,
                    groups = class_groups, note_size = "",
                    note = regression_note(reg_mistakes_class_models, slider_class_data, "full", class_covariates))

## ----- Regression (3): sets completed -----

reg_sets_class_basic <- lm_robust(sets_completed ~ treatment + criterion, slider_class_data)

reg_sets_class_baselines <- lm_robust(sets_completed ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_class_data)

reg_sets_class_covariates <- lm_robust(sets_completed ~ treatment + criterion + gender + race + income + mouse, slider_class_data)

reg_sets_class_full <- lm_robust(sets_completed ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + mouse, slider_class_data)

reg_sets_class_models <- list(reg_sets_class_basic, reg_sets_class_covariates, reg_sets_class_baselines, reg_sets_class_full)

save_regression_tex(reg_sets_class_models, "output/tables/reg_sets_class.tex",
                    outcome = "Sets completed", coef_map = class_coef_labels,
                    groups = class_groups, note_size = "",
                    note = regression_note(reg_sets_class_models, slider_class_data, "full", class_covariates))

## ----- Tests of equality of treatment coefficients -----

# Treatments 2 vs 3 (models 3 and 4)

coefficient_tests_class <- tibble(
  outcome = rep(c("Accuracy target", "Mistake rate", "Sets completed"), each = 2),
  model = rep(c("Model 3", "Model 4"), times = 3),
  p.value = c(lh_p_value(reg_target_class_baselines, "treatment2 = treatment3"),
              lh_p_value(reg_target_class_full, "treatment2 = treatment3"),
              lh_p_value(reg_mistakes_class_baselines, "treatment2 = treatment3"),
              lh_p_value(reg_mistakes_class_full, "treatment2 = treatment3"),
              lh_p_value(reg_sets_class_baselines, "treatment2 = treatment3"),
              lh_p_value(reg_sets_class_full, "treatment2 = treatment3")))

print(coefficient_tests_class)

write_csv(coefficient_tests_class, "results/numbers_in_paper/coefficient_tests_class.csv")

# ----- Numbers cited in the text -----

# Treatment 2 vs 3 coefficient tests (e.g. CoefTestLabAccuracyTargetModelThreePval)

write_tex_macros(c(p_value_macros(str_c(coefficient_tests_lab$outcome, " ", coefficient_tests_lab$model),
                                  coefficient_tests_lab$p.value, "CoefTestLab"),
                   p_value_macros(str_c(coefficient_tests_class$outcome, " ", coefficient_tests_class$model),
                                  coefficient_tests_class$p.value, "CoefTestClass")),
                 "output/numbers/regression_numbers_appendix.tex")
