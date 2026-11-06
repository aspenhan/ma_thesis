# Regression estimates of treatment effects on effort (pooled sample): regressions (1)-(3) on accuracy
# target achievement, mistake rate and sets completed, tests of equality of treatment coefficients,
# and regressions (4)-(6) interacting treatment 2 with loss aversion
# Outputs: output/tables/reg_*.tex (main text tables and full appendix tables), output/numbers/regression_numbers.tex,
#          results/numbers_in_paper/coefficient_tests_pooled.csv, results/numbers_in_paper/regression_sense_checks.csv
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

## ----- Recode covariates for the regressions (see prepare_regression_covariates in setup.R) -----

slider_data <- prepare_regression_covariates(slider_data)

## ----- Create variable for target achievement -----

slider_data <- slider_data %>%
  mutate(accuracy_target = if_else(recorded_mistake_rate <= 10, "Yes", "No"),
         accuracy_target = factor(accuracy_target, levels = c("No", "Yes")))

## ----- Table options -----

# Row labels for all coefficients, and the blocks of rows shown under a heading in the full (appendix) tables

coef_labels <- regression_coef_map(slider_data)

full_groups <- regression_groups(slider_data)

# Covariates in the specifications that control for them

pooled_covariates <- c("gender", "race", "income", "edu", "econ", "occupation", "mouse")

# Main text tables only report treatment effects (no constant), flagging which controls are included

main_coefs <- coef_labels[c("treatment2", "treatment3", "treatment4")]

main_control_rows <- list("Baselines" = c("No", "No", "Yes", "Yes"),
                          "Covariates" = c("No", "Yes", "No", "Yes"))

# Outcome of the accuracy target regressions, described in their table notes

target_note <- "The outcome is meeting the accuracy target, that is, a recorded mistake rate of at most 10\\%."

# ----- Regression (1): accuracy target achievement -----

## ----- Create regression models -----

reg_target_basic <- glm(accuracy_target ~ treatment + criterion, family = binomial, data = slider_data)

reg_target_baselines <- glm(accuracy_target ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, family = binomial, slider_data)

reg_target_covariates <- glm(accuracy_target ~ treatment + criterion + gender + race + income + edu + econ + occupation + mouse, family = binomial, slider_data)

reg_target_full <- glm(accuracy_target ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, family = binomial, slider_data)

reg_target_models <- list(reg_target_basic, reg_target_covariates, reg_target_baselines, reg_target_full)

## ----- Create regression tables (main text and appendix) -----

save_regression_tex(reg_target_models, "output/tables/reg_target.tex",
                    outcome = "Log odds of meeting the accuracy target", coef_map = main_coefs,
                    control_rows = main_control_rows,
                    note = regression_note(reg_target_models, slider_data, "main", pooled_covariates, target_note))

save_regression_tex(reg_target_models, "output/tables/reg_target_full.tex",
                    outcome = "Log odds of meeting the accuracy target", coef_map = coef_labels,
                    groups = full_groups, note_size = "",
                    note = regression_note(reg_target_models, slider_data, "full", pooled_covariates, target_note))

# ----- Regression (2): mistake rate -----

## ----- Create regression models -----

reg_mistakes_basic <- lm_robust(mistake_rate ~ treatment + criterion, slider_data, se_type = robust_se_type)

reg_mistakes_baselines <- lm_robust(mistake_rate ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_data, se_type = robust_se_type)

reg_mistakes_covariates <- lm_robust(mistake_rate ~ treatment + criterion + gender + race + income + edu + econ + occupation + mouse, slider_data, se_type = robust_se_type)

reg_mistakes_full <- lm_robust(mistake_rate ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, slider_data, se_type = robust_se_type)

reg_mistakes_models <- list(reg_mistakes_basic, reg_mistakes_covariates, reg_mistakes_baselines, reg_mistakes_full)

## ----- Create regression tables (main text and appendix) -----

save_regression_tex(reg_mistakes_models, "output/tables/reg_mistakes.tex",
                    outcome = "Mistake rate (\\%)", coef_map = main_coefs,
                    control_rows = main_control_rows,
                    note = regression_note(reg_mistakes_models, slider_data, "main", pooled_covariates))

save_regression_tex(reg_mistakes_models, "output/tables/reg_mistakes_full.tex",
                    outcome = "Mistake rate (\\%)", coef_map = coef_labels,
                    groups = full_groups, note_size = "",
                    note = regression_note(reg_mistakes_models, slider_data, "full", pooled_covariates))

# ----- Regression (3): sets completed -----

## ----- Create regression models -----

reg_sets_basic <- lm_robust(sets_completed ~ treatment + criterion, slider_data, se_type = robust_se_type)

reg_sets_baselines <- lm_robust(sets_completed ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_data, se_type = robust_se_type)

reg_sets_covariates <- lm_robust(sets_completed ~ treatment + criterion + gender + race + income + edu + econ + occupation + mouse, slider_data, se_type = robust_se_type)

reg_sets_full <- lm_robust(sets_completed ~ treatment + criterion + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, slider_data, se_type = robust_se_type)

reg_sets_models <- list(reg_sets_basic, reg_sets_covariates, reg_sets_baselines, reg_sets_full)

## ----- Create regression tables (main text and appendix) -----

save_regression_tex(reg_sets_models, "output/tables/reg_sets.tex",
                    outcome = "Sets completed", coef_map = main_coefs,
                    control_rows = main_control_rows,
                    note = regression_note(reg_sets_models, slider_data, "main", pooled_covariates))

save_regression_tex(reg_sets_models, "output/tables/reg_sets_full.tex",
                    outcome = "Sets completed", coef_map = coef_labels,
                    groups = full_groups, note_size = "",
                    note = regression_note(reg_sets_models, slider_data, "full", pooled_covariates))

# ----- Tests of equality of treatment coefficients -----

# Treatments 2 vs 3, in the specifications that control for baseline performance, without and with covariates

coefficient_tests_pooled <- tibble(
  outcome = rep(c("Accuracy target", "Mistake rate", "Sets completed"), each = 2),
  model = rep(c("Baselines", "Baselines and covariates"), times = 3),
  p.value = c(lh_p_value(reg_target_baselines, "treatment2 = treatment3"),
              lh_p_value(reg_target_full, "treatment2 = treatment3"),
              lh_p_value(reg_mistakes_baselines, "treatment2 = treatment3"),
              lh_p_value(reg_mistakes_full, "treatment2 = treatment3"), #unestimable given NaN standard errors
              lh_p_value(reg_sets_baselines, "treatment2 = treatment3"),
              lh_p_value(reg_sets_full, "treatment2 = treatment3")))

print(coefficient_tests_pooled)

write_with_retry(function() write_csv(coefficient_tests_pooled, "results/numbers_in_paper/coefficient_tests_pooled.csv"))

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

la_coefs <- coef_labels[c("treatment2", "treatment3", "treatment4", "criterionstrict",
                          "lambda_i_adj", "treatment2_lambda_i_adj", "(Intercept)")]

la_control_rows <- list("Baselines" = c("Yes", "Yes"),
                        "Covariates" = c("No", "Yes"))

## ----- Regression (4): accuracy target achievement -----

# Only treatment 2 is interacted with loss aversion, as in regression (4) and the table reported in the manuscript
# (the quarto chunk's treatment*lambda_i_adj would interact every treatment)

reg_target_la_baselines <- glm(accuracy_target ~ treatment + criterion + lambda_i_adj + treatment2_lambda_i_adj + demo_sets_attempted + demo_sets_completed + demo_mistakes, family = binomial, slider_data)

reg_target_la_full <- glm(accuracy_target ~ treatment + criterion + lambda_i_adj + treatment2_lambda_i_adj + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, family = binomial, slider_data)

reg_target_la_models <- list(reg_target_la_baselines, reg_target_la_full)

save_regression_tex(reg_target_la_models, "output/tables/reg_target_la.tex",
                    outcome = "Log odds of meeting the accuracy target", coef_map = la_coefs,
                    control_rows = la_control_rows,
                    note = regression_note(reg_target_la_models, slider_data, "la", pooled_covariates, target_note))

## ----- Regression (5): mistake rate -----

reg_mistakes_la_baselines <- lm_robust(mistake_rate ~ treatment + criterion + lambda_i_adj + treatment2_lambda_i_adj + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_data, se_type = robust_se_type)

reg_mistakes_la_full <- lm_robust(mistake_rate ~ treatment + criterion + lambda_i_adj + treatment2_lambda_i_adj + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, slider_data, se_type = robust_se_type)

reg_mistakes_la_models <- list(reg_mistakes_la_baselines, reg_mistakes_la_full)

save_regression_tex(reg_mistakes_la_models, "output/tables/reg_mistakes_la.tex",
                    outcome = "Mistake rate (\\%)", coef_map = la_coefs,
                    control_rows = la_control_rows,
                    note = regression_note(reg_mistakes_la_models, slider_data, "la", pooled_covariates))

## ----- Regression (6): sets completed -----

reg_sets_la_baselines <- lm_robust(sets_completed ~ treatment + criterion + lambda_i_adj + treatment2_lambda_i_adj + demo_sets_attempted + demo_sets_completed + demo_mistakes, slider_data, se_type = robust_se_type)

reg_sets_la_full <- lm_robust(sets_completed ~ treatment + criterion + lambda_i_adj + treatment2_lambda_i_adj + demo_sets_attempted + demo_sets_completed + demo_mistakes + gender + race + income + edu + econ + occupation + mouse, slider_data, se_type = robust_se_type)

reg_sets_la_models <- list(reg_sets_la_baselines, reg_sets_la_full)

save_regression_tex(reg_sets_la_models, "output/tables/reg_sets_la.tex",
                    outcome = "Sets completed", coef_map = la_coefs,
                    control_rows = la_control_rows,
                    note = regression_note(reg_sets_la_models, slider_data, "la", pooled_covariates))

## ----- Loss aversion sense checks -----

# Loss aversion estimates beyond the top and bottom-coding bounds

regression_sense_checks <- regression_sense_checks %>%
  mutate(lambda_above_bound = nrow(slider_data %>% filter(lambda_i > 4.33)),
         lambda_below_bound = nrow(slider_data %>% filter(lambda_i < 0)),
         lambda_estimated = nrow(slider_data %>% filter(!is.na(lambda_i))))

regression_sense_checks %>%
  pivot_longer(everything()) %>%
  print(n = Inf)

write_with_retry(function() write_csv(regression_sense_checks, "results/numbers_in_paper/regression_sense_checks.csv"))

# ----- Numbers cited in the text -----

write_tex_macros(c(

  # Mean sets completed in the control

  list(ControlMeanSets = round(mean(slider_data$sets_completed[slider_data$treatment == 1]), 2)),

  # Adjusted R-squared of the sets completed regressions without controls and with baselines (no longer in the tables)

  list(AdjRsqSetsNoControls = sprintf("%.2f", reg_sets_basic$adj.r.squared),
       AdjRsqSetsBaselines = sprintf("%.2f", reg_sets_baselines$adj.r.squared)),

  # Treatment 2 vs 3 coefficient tests (e.g. CoefTestPooledAccuracyTargetBaselinesPval)

  p_value_macros(str_c(coefficient_tests_pooled$outcome, " ", coefficient_tests_pooled$model),
                 coefficient_tests_pooled$p.value, "CoefTestPooled"),

  # Cognitive complexity treatment sense checks

  with(regression_sense_checks, list(
    ExplanationNotHelpful = explanation_not_helpful,
    ExplanationResponses = explanation_responses,
    InstructionsTimeTreatmentOne = sprintf("%.2f", treatment1_instructions_time),
    InstructionsTimeTreatmentTwo = sprintf("%.2f", treatment2_instructions_time),
    InstructionsTimeTreatmentThree = sprintf("%.2f", treatment3_instructions_time),
    InstructionsTimeTreatmentFour = sprintf("%.2f", treatment4_instructions_time),

    # Loss aversion sense checks

    LambdaEstimated = lambda_estimated,
    LambdaOutOfBounds = lambda_above_bound + lambda_below_bound,
    LambdaAboveBound = lambda_above_bound,
    LambdaBelowBound = lambda_below_bound))),

  "output/numbers/regression_numbers.tex")
