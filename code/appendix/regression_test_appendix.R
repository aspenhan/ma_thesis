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

## ----- Regression (1): accuracy target achievement -----

reg_target_lab_basic <- glm(accuracy_target ~ treatment + criterion, family = binomial, data = slider_lab_data)

reg_target_lab_baselines <- glm(accuracy_target ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, family = binomial, slider_lab_data)

reg_target_lab_covariates <- glm(accuracy_target ~ treatment + criterion + gender + race + income + edu + econ + occupation + mouse, family = binomial, slider_lab_data)

reg_target_lab_full <- glm(accuracy_target ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, family = binomial, slider_lab_data)

reg_target_lab_models <- list(reg_target_lab_basic, reg_target_lab_covariates, reg_target_lab_baselines, reg_target_lab_full)

reg_target_lab_tbl <- texreg(reg_target_lab_models, digits = 2,
                         stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                         custom.header = list("Log odds of achieving accuracy target" = 1:4),
                         include.deviance = FALSE,
                         table = FALSE)

save_texreg_tex(reg_target_lab_tbl, "output/tables/reg_target_lab.tex")

## ----- Regression (2): mistake rate -----

reg_mistakes_lab_basic <- lm_robust(mistake_rate ~ treatment + criterion, slider_lab_data)

reg_mistakes_lab_baselines <- lm_robust(mistake_rate ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_lab_data)

reg_mistakes_lab_covariates <- lm_robust(mistake_rate ~ treatment + criterion + gender + race + income + edu + econ + occupation + mouse, slider_lab_data)

reg_mistakes_lab_full <- lm_robust(mistake_rate ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, slider_lab_data)

reg_mistakes_lab_models <- list(reg_mistakes_lab_basic, reg_mistakes_lab_covariates, reg_mistakes_lab_baselines, reg_mistakes_lab_full)

reg_mistakes_lab_tbl <- texreg(reg_mistakes_lab_models, digits = 2,
                               stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                               custom.header = list("Mistake rate" = 1:4),
                               include.deviance = FALSE,
                               table = FALSE)

save_texreg_tex(reg_mistakes_lab_tbl, "output/tables/reg_mistakes_lab.tex")

## ----- Regression (3): sets completed -----

reg_sets_lab_basic <- lm_robust(sets_completed ~ treatment + criterion, slider_lab_data)

reg_sets_lab_baselines <- lm_robust(sets_completed ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_lab_data)

reg_sets_lab_covariates <- lm_robust(sets_completed ~ treatment + criterion + gender + race + income + edu + econ + occupation + mouse, slider_lab_data)

reg_sets_lab_full <- lm_robust(sets_completed ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, slider_lab_data)

reg_sets_lab_models <- list(reg_sets_lab_basic, reg_sets_lab_covariates, reg_sets_lab_baselines, reg_sets_lab_full)

reg_sets_lab_tbl <- texreg(reg_sets_lab_models, digits = 2,
                         stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                         custom.header = list("Sets completed" = 1:4),
                         include.deviance = FALSE,
                         table = FALSE)

save_texreg_tex(reg_sets_lab_tbl, "output/tables/reg_sets_lab.tex")

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

## ----- Regression (1): accuracy target achievement -----

reg_target_class_basic <- glm(accuracy_target ~ treatment + criterion, family = binomial, data = slider_class_data)

reg_target_class_baselines <- glm(accuracy_target ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, family = binomial, slider_class_data)

reg_target_class_covariates <- glm(accuracy_target ~ treatment + criterion + gender + race + income + mouse, family = binomial, slider_class_data)

reg_target_class_full <- glm(accuracy_target ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + mouse, family = binomial, slider_class_data)

reg_target_class_models <- list(reg_target_class_basic, reg_target_class_covariates, reg_target_class_baselines, reg_target_class_full)

reg_target_class_tbl <- texreg(reg_target_class_models, digits = 2,
                         stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                         custom.header = list("Log odds of achieving accuracy target" = 1:4),
                         include.deviance = FALSE,
                         table = FALSE)

save_texreg_tex(reg_target_class_tbl, "output/tables/reg_target_class.tex")

## ----- Regression (2): mistake rate -----

reg_mistakes_class_basic <- lm_robust(mistake_rate ~ treatment + criterion, slider_class_data)

reg_mistakes_class_baselines <- lm_robust(mistake_rate ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_class_data)

reg_mistakes_class_covariates <- lm_robust(mistake_rate ~ treatment + criterion + gender + race + income + mouse, slider_class_data)

reg_mistakes_class_full <- lm_robust(mistake_rate ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + mouse, slider_class_data)

reg_mistakes_class_models <- list(reg_mistakes_class_basic, reg_mistakes_class_covariates, reg_mistakes_class_baselines, reg_mistakes_class_full)

reg_mistakes_class_tbl <- texreg(reg_mistakes_class_models, digits = 2,
                         stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                         custom.header = list("Mistake rate" = 1:4),
                         include.deviance = FALSE,
                         table = FALSE)

save_texreg_tex(reg_mistakes_class_tbl, "output/tables/reg_mistakes_class.tex")

## ----- Regression (3): sets completed -----

reg_sets_class_basic <- lm_robust(sets_completed ~ treatment + criterion, slider_class_data)

reg_sets_class_baselines <- lm_robust(sets_completed ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_class_data)

reg_sets_class_covariates <- lm_robust(sets_completed ~ treatment + criterion + gender + race + income + mouse, slider_class_data)

reg_sets_class_full <- lm_robust(sets_completed ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + mouse, slider_class_data)

reg_sets_class_models <- list(reg_sets_class_basic, reg_sets_class_covariates, reg_sets_class_baselines, reg_sets_class_full)

reg_sets_class_tbl <- texreg(reg_sets_class_models, digits = 2,
                         stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                         custom.header = list("Sets completed" = 1:4),
                         include.deviance = FALSE,
                         table = FALSE)

save_texreg_tex(reg_sets_class_tbl, "output/tables/reg_sets_class.tex")

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
