# Regression estimates of treatment effects on effort (pooled sample): regressions (1)-(3) on accuracy
# target achievement, mistake rate and sets completed, tests of equality of treatment coefficients,
# and regressions (4)-(6) interacting treatment 2 with loss aversion
# Outputs: output/tables/reg_*.tex (main text tables and full appendix tables), output/numbers/regression_numbers.tex,
#          output/tests/coefficient_tests_pooled.csv, output/tests/regression_sense_checks.csv
# Lab and class sample counterparts are in code/appendix/regression_test_appendix.R

# ----- Setup -----

# Load packages and helpers, unless already done by code/run_all.R

if (!exists("run_all")) source("code/setup.R")

# ----- Load data -----

# Cleaned data with loss aversion estimates from code/loss_aversion_estimation.R

slider_data <- read_rds("data/slider_full_data_loss_aversion.rds")

# ----- Prepare data -----

## ----- Check target achievements -----

# No subject achieved the speed target, so only the accuracy target is modelled

regression_sense_checks <- tibble(max_sets_completed = max(slider_data$sets_completed),
                                  min_mistake_rate = min(slider_data$mistake_rate))

## ----- Relevel education -----

slider_data$edu <- relevel(slider_data$edu, ref = "College degree or above")

## ----- Create variable for target achievement -----

slider_data <- slider_data %>%
  mutate(accuracy_target = if_else(recorded_mistake_rate <= 10, "Yes", "No"),
         accuracy_target = factor(accuracy_target, levels = c("No", "Yes")))

## ----- Main text table options -----

# Main text tables only report treatment effects, flagging which controls are included

main_omit_coef <- "criterion|gender|race|income|edu|econ|occupation|mouse|demo_"

main_control_rows <- list("Baselines" = c("No", "No", "Yes", "Yes"),
                          "Covariates" = c("No", "Yes", "No", "Yes"))

# ----- Regression (1): accuracy target achievement -----

## ----- Create regression models -----

reg_target_basic <- glm(accuracy_target ~ treatment + criterion, family = binomial, data = slider_data)

reg_target_baselines <- glm(accuracy_target ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, family = binomial, slider_data)

reg_target_covariates <- glm(accuracy_target ~ treatment + criterion + gender + race + income + edu + econ + occupation + mouse, family = binomial, slider_data)

reg_target_full <- glm(accuracy_target ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, family = binomial, slider_data)

reg_target_models <- list(reg_target_basic, reg_target_covariates, reg_target_baselines, reg_target_full)

## ----- Create regression tables (main text and appendix) -----

reg_target_tbl <- texreg(reg_target_models, digits = 2,
                         stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                         custom.header = list("Log odds of achieving accuracy target" = 1:4),
                         include.deviance = FALSE,
                         omit.coef = main_omit_coef,
                         custom.gof.rows = main_control_rows,
                         table = FALSE)

save_texreg_tex(reg_target_tbl, "output/tables/reg_target.tex")

reg_target_full_tbl <- texreg(reg_target_models, digits = 2,
                              stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                              custom.header = list("Log odds of achieving accuracy target" = 1:4),
                              include.deviance = FALSE,
                              table = FALSE)

save_texreg_tex(reg_target_full_tbl, "output/tables/reg_target_full.tex")

# ----- Regression (2): mistake rate -----

## ----- Create regression models -----

reg_mistakes_basic <- lm_robust(mistake_rate ~ treatment + criterion, slider_data)

reg_mistakes_baselines <- lm_robust(mistake_rate ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_data)

reg_mistakes_covariates <- lm_robust(mistake_rate ~ treatment + criterion + gender + race + income + edu + econ + occupation + mouse, slider_data)

reg_mistakes_full <- lm_robust(mistake_rate ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, slider_data)

reg_mistakes_models <- list(reg_mistakes_basic, reg_mistakes_covariates, reg_mistakes_baselines, reg_mistakes_full)

## ----- Create regression tables (main text and appendix) -----

reg_mistakes_tbl <- texreg(reg_mistakes_models, digits = 2,
                         stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                         custom.header = list("Mistake rate" = 1:4),
                         include.deviance = FALSE,
                         omit.coef = main_omit_coef,
                         custom.gof.rows = main_control_rows,
                         table = FALSE)

save_texreg_tex(reg_mistakes_tbl, "output/tables/reg_mistakes.tex")

reg_mistakes_full_tbl <- texreg(reg_mistakes_models, digits = 2,
                                stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                                custom.header = list("Mistake rate" = 1:4),
                                include.deviance = FALSE,
                                table = FALSE)

save_texreg_tex(reg_mistakes_full_tbl, "output/tables/reg_mistakes_full.tex")

# ----- Regression (3): sets completed -----

## ----- Create regression models -----

reg_sets_basic <- lm_robust(sets_completed ~ treatment + criterion, slider_data)

reg_sets_baselines <- lm_robust(sets_completed ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_data)

reg_sets_covariates <- lm_robust(sets_completed ~ treatment + criterion + gender + race + income + edu + econ + occupation + mouse, slider_data)

reg_sets_full <- lm_robust(sets_completed ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, slider_data)

reg_sets_models <- list(reg_sets_basic, reg_sets_covariates, reg_sets_baselines, reg_sets_full)

## ----- Create regression tables (main text and appendix) -----

reg_sets_tbl <- texreg(reg_sets_models, digits = 2,
                         stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                         custom.header = list("Sets completed" = 1:4),
                         include.deviance = FALSE,
                         omit.coef = main_omit_coef,
                         custom.gof.rows = main_control_rows,
                         table = FALSE)

save_texreg_tex(reg_sets_tbl, "output/tables/reg_sets.tex")

reg_sets_full_tbl <- texreg(reg_sets_models, digits = 2,
                            stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                            custom.header = list("Sets completed" = 1:4),
                            include.deviance = FALSE,
                            table = FALSE)

save_texreg_tex(reg_sets_full_tbl, "output/tables/reg_sets_full.tex")

# ----- Tests of equality of treatment coefficients -----

# Treatments 2 vs 3 (models 3 and 4)

coefficient_tests_pooled <- tibble(
  outcome = rep(c("Accuracy target", "Mistake rate", "Sets completed"), each = 2),
  model = rep(c("Model 3", "Model 4"), times = 3),
  p.value = c(lh_p_value(reg_target_baselines, "treatment2 = treatment3"),
              lh_p_value(reg_target_full, "treatment2 = treatment3"),
              lh_p_value(reg_mistakes_baselines, "treatment2 = treatment3"),
              lh_p_value(reg_mistakes_full, "treatment2 = treatment3"), #unestimable given NaN standard errors
              lh_p_value(reg_sets_baselines, "treatment2 = treatment3"),
              lh_p_value(reg_sets_full, "treatment2 = treatment3")))

coefficient_tests_pooled

write_csv(coefficient_tests_pooled, "output/tests/coefficient_tests_pooled.csv")

# ----- Cognitive complexity treatment sense checks -----

regression_sense_checks <- regression_sense_checks %>%
  mutate(

    # Did explanation help?

    explanation_not_helpful = nrow(slider_data %>% filter(targets_explanation_help == "No")),
    explanation_responses = nrow(slider_data %>% filter(!is.na(targets_explanation_help))),

    # Instructions time spent

    treatment1_instructions_time = mean(slider_data$treatment1_instructions_time, na.rm = TRUE),
    treatment2_instructions_time = mean(slider_data$treatment2_instructions_time, na.rm = TRUE),
    treatment3_instructions_time = mean(slider_data$treatment3_instructions_time, na.rm = TRUE),
    treatment4_instructions_time = mean(slider_data$treatment4_instructions_time, na.rm = TRUE)

    )

# ----- Verification of reference point effects: interaction with loss aversion -----

## ----- Prepare interaction term and table options -----

# Create a interaction term for treatment 2 and loss aversion

slider_data <- slider_data %>%
  mutate(treatment2_lambda_i_adj = if_else(treatment == 2, lambda_i_adj, 0))

# Loss aversion tables keep the criterion and loss aversion terms

la_omit_coef <- "gender|race|income|edu|econ|occupation|mouse|demo_"

la_control_rows <- list("Baselines" = c("Yes", "Yes"),
                        "Covariates" = c("No", "Yes"))

## ----- Regression (4): accuracy target achievement -----

# Only treatment 2 is interacted with loss aversion, as in regression (4) and the table reported in the manuscript
# (the quarto chunk's treatment*lambda_i_adj would interact every treatment)

reg_target_la_baselines <- glm(accuracy_target ~ treatment + criterion + lambda_i_adj + treatment2_lambda_i_adj + demo_sets_attempted + demo_sets_completed + demo_mistakes, family = binomial, slider_data)

reg_target_la_full <- glm(accuracy_target ~ treatment + criterion + lambda_i_adj + treatment2_lambda_i_adj + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, family = binomial, slider_data)

reg_target_la_models <- list(reg_target_la_baselines, reg_target_la_full)

reg_target_la_tbl <- texreg(reg_target_la_models, digits = 2,
                         stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                         custom.header = list("Log odds of achieving accuracy target" = 1:2),
                         custom.model.names = c("Model 3", "Model 4"),
                         include.deviance = FALSE,
                         omit.coef = la_omit_coef,
                         custom.gof.rows = la_control_rows,
                         table = FALSE)

save_texreg_tex(reg_target_la_tbl, "output/tables/reg_target_la.tex")

## ----- Regression (5): mistake rate -----

reg_mistakes_la_baselines <- lm_robust(mistake_rate ~ treatment + criterion + lambda_i_adj + treatment2_lambda_i_adj + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_data)

reg_mistakes_la_full <- lm_robust(mistake_rate ~ treatment + criterion + lambda_i_adj + treatment2_lambda_i_adj + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, slider_data)

reg_mistakes_la_models <- list(reg_mistakes_la_baselines, reg_mistakes_la_full)

reg_mistakes_la_tbl <- texreg(reg_mistakes_la_models, digits = 2,
                         stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                         custom.header = list("Mistake rate" = 1:2),
                         custom.model.names = c("Model 3", "Model 4"),
                         include.deviance = FALSE,
                         omit.coef = la_omit_coef,
                         custom.gof.rows = la_control_rows,
                         table = FALSE)

save_texreg_tex(reg_mistakes_la_tbl, "output/tables/reg_mistakes_la.tex")

## ----- Regression (6): sets completed -----

reg_sets_la_baselines <- lm_robust(sets_completed ~ treatment + criterion + lambda_i_adj + treatment2_lambda_i_adj + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_data)

reg_sets_la_full <- lm_robust(sets_completed ~ treatment + criterion + lambda_i_adj + treatment2_lambda_i_adj + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, slider_data)

reg_sets_la_models <- list(reg_sets_la_baselines, reg_sets_la_full)

reg_sets_la_tbl <- texreg(reg_sets_la_models, digits = 2,
                         stars = c(0.001, 0.01, 0.05), include.ci = FALSE,
                         custom.header = list("Sets completed" = 1:2),
                         custom.model.names = c("Model 3", "Model 4"),
                         include.deviance = FALSE,
                         omit.coef = la_omit_coef,
                         custom.gof.rows = la_control_rows,
                         table = FALSE)

save_texreg_tex(reg_sets_la_tbl, "output/tables/reg_sets_la.tex")

## ----- Loss aversion sense checks -----

# Loss aversion estimates beyond the top and bottom-coding bounds

regression_sense_checks <- regression_sense_checks %>%
  mutate(lambda_above_bound = nrow(slider_data %>% filter(lambda_i > 4.33)),
         lambda_below_bound = nrow(slider_data %>% filter(lambda_i < 0)),
         lambda_estimated = nrow(slider_data %>% filter(!is.na(lambda_i))))

regression_sense_checks %>%
  pivot_longer(everything())

write_csv(regression_sense_checks, "output/tests/regression_sense_checks.csv")

# ----- Numbers cited in the text -----

write_tex_macros(list(ControlMeanSets = round(mean(slider_data$sets_completed[slider_data$treatment == 1]), 2)),
                 "output/numbers/regression_numbers.tex")
