# Shared setup for the pipeline: packages, plot theme, output folders, and helper functions which
# export output used by thesis_manuscript_latex.tex
# Sourced by code/run_all.R, or by each script when it is run on its own
# All scripts assume the working directory is the project root (e.g. open ma_thesis.Rproj)

# ----- Packages -----

## ----- Data reading and writing, transformation, and visualisation -----

library(tidyverse)
library(readxl) #read excel data
library(broom) #turns function output into tidy tibbles

## ----- Statistical analysis -----

library(DescTools) #descriptive stats
library(car) #basic regression analysis
library(sandwich) #estimate robust errors
library(estimatr) #run robust and other more complex regressions and analysis
library(nnet) #run multinomial logistic regressions
library(fixest) #run fixed effects regressions
library(lmtest) #run diagnostic tests on linear models

## ----- Tables -----

library(texreg) #make regression tables
library(gtsummary) #make summary stats tables
library(gt) #make general tables

# ----- Plot theme -----

# Set default theme for ggplot; serif is times new roman

def_theme <- theme(plot.title = element_text(family = "serif", size = 13, hjust = 0.5),
                  axis.title = element_text(family = "serif", size = 12),
                  axis.text = element_text(family = "serif", size = 11),
                  strip.text.x = element_text(family = "serif", size = 12),
                  strip.text.y = element_text(family = "serif", size = 12, angle = 0),
                  legend.title = element_text(family = "serif", size = 12),
                  legend.text = element_text(family = "serif", size = 11))

# ----- Labels for tables and figures -----

## ----- Treatment groups -----

# Names of treatments 1-4 as defined in the manuscript (\CtrlName, \LPName, \SPName and \LNName in manuscript_megan_edits.tex)

treatment_names <- c("1" = "Control", "2" = "Lenient with Plan", "3" = "Strict with Plan", "4" = "Lenient without Plan")

## ----- Covariates -----

covariate_names <- c(gender = "Gender", race = "Race", age = "Age", income = "Household income",
                     edu = "Education level", econ = "Studied economics", occupation = "Occupation", mouse = "Used mouse")

# Clearer names for some covariate categories ("--" is an en dash in LaTeX; gt escapes the dollar signs, and
# latex_dollars() escapes them for texreg tables and notes)

covariate_level_names <- list(
  income = c("0 - 24,999" = "$0--24,999", "25,000 - 49,999" = "$25,000--49,999", "50,000 - 74,999" = "$50,000--74,999",
             "75,000 - 119,999" = "$75,000--119,999", "120,000 - 199,999" = "$120,000--199,999",
             "200,000 and over" = "$200,000 and over"),
  occupation = c("High-tech mfg or eng" = "High-tech manufacturing or engineering"))

latex_dollars <- function(x) str_replace_all(x, fixed("$"), "\\$")

# Rename covariate categories for display (levels keep their order, so tests and regressions are unchanged)

relabel_covariates <- function(data) {
  for (var in intersect(names(covariate_level_names), names(data))) {
    lookup <- covariate_level_names[[var]]
    lvls <- levels(data[[var]])
    levels(data[[var]]) <- if_else(lvls %in% names(lookup), unname(lookup[lvls]), lvls)
  }
  data
}

## ----- Significance stars (used in every table) -----

star_levels <- c(0.01, 0.05, 0.1)

star_note <- "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."

# ----- Output folders read by the LaTeX manuscript -----

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/numbers", recursive = TRUE, showWarnings = FALSE)

# ----- Results folders for datasets of analysis results -----

# Full tables of the tests and checks behind the numbers cited in the text (the numbers themselves go in output/numbers)

dir.create("results/numbers_in_paper", recursive = TRUE, showWarnings = FALSE)

# ----- Helper functions for exporting output -----

## ----- Writing files -----

# Run a function that writes a file, retrying for a few seconds if the file is locked (backup or antivirus software
# can briefly hold an output file open just after it is written)

write_with_retry <- function(write_fn, tries = 10) {
  for (i in seq_len(tries)) {
    result <- tryCatch({
      write_fn()
      NULL
    }, error = function(e) e)
    if (is.null(result)) return(invisible(NULL))
    if (i == tries) stop(result)
    Sys.sleep(0.5)
  }
}

## ----- Tables -----

### ----- Descriptive tables (gtsummary/gt) -----

# Plain-text placeholders in gt table text, swapped for LaTeX when saving (gt escapes LaTeX commands)

gt_latex_tokens <- c("NOTESLABEL" = "\\\\textit{Notes:}", "CHISQUARED" = "$\\\\chi^2$")

# Column headers for the treatment groups, broken over two lines so that wide tables fit the page

treatment_headers <- c("Lenient with Plan" = "Lenient\\\\\\\\with Plan", "Strict with Plan" = "Strict\\\\\\\\with Plan",
                       "Lenient without Plan" = "Lenient\\\\\\\\without Plan")

# Add a final row with the number of participants in each column (all participants, then each treatment group)

add_n_row <- function(tbl, data, suffix = "") {
  counts <- c(nrow(data), as.vector(table(data$treatment)))
  n_row <- tibble(variable = "n_obs", row_type = "label", label = "Observations",
                  !!!set_names(as.list(as.character(counts)), str_c("stat_", 0:4, suffix)))
  modify_table_body(tbl, ~ bind_rows(.x, n_row))
}

# Convert a gtsummary table to gt, replacing gtsummary's footnotes with a single table note

finish_gt <- function(tbl, note) {
  tbl %>%
    as_gt() %>%
    rm_footnotes() %>%
    rm_source_notes() %>%
    tab_source_note(str_c("NOTESLABEL ", note))
}

# Set in bold the first cell of each row that heads a block of indented rows (e.g. "Gender" above its categories)

bold_group_headings <- function(tex) {
  lines <- str_split(tex, "\n")[[1]]
  is_indented <- str_detect(lines, "^\\\\quad ")
  heading <- !is_indented & lead(is_indented, default = FALSE) & str_detect(lines, "&")
  lines[heading] <- str_replace(lines[heading], "^([^&]*?)\\s*&", "\\\\textbf{\\1} &")
  str_c(lines, collapse = "\n")
}

# Save a gt table as LaTeX, dropping gt's table environment since the manuscript supplies the float, caption and label.
# Also indents category rows with \quad instead of gtsummary's non-breaking spaces, sets the headings of blocks of
# categories in bold, breaks the treatment group headers over two lines, and adds a rule above the observations row.
# In tables with panels (row groups), the panel headings are set in bold below a single rule instead

save_gt_tex <- function(gt_table, file) {
  group_heading <- "\\\\midrule\\\\addlinespace\\[2.5pt\\]\n\\\\multicolumn\\{(\\d+)\\}\\{l\\}\\{([^}]*)\\} \\\\\\\\\\[2.5pt\\] \n\\\\midrule\\\\addlinespace\\[2.5pt\\]\n"
  tex <- gt_table %>%
    as_latex() %>%
    as.character() %>%
    str_remove("^\\s*\\\\begin\\{table\\}(\\[[^\\]]*\\])?\\s*") %>%
    str_remove("\\s*\\\\end\\{table\\}\\s*$") %>%
    str_replace_all(gt_latex_tokens) %>%
    str_replace_all("(?m)^[ \\u00a0]{4}(?=\\S)", "\\\\quad ") %>% #gtsummary indents with non-breaking spaces
    bold_group_headings() %>%
    str_replace(fixed(">{\\raggedright\\arraybackslash}p{"),
                fixed(">{\\raggedright\\arraybackslash\\hangindent=2em}p{")) %>% #indent wrapped row labels
    str_replace("\\\\\\\\\n\\\\end\\{minipage\\}", "\n\\\\end{minipage}") #line break after the note
  header_end <- str_locate(tex, fixed("\\midrule"))[1, "start"]
  header <- str_sub(tex, 1, header_end - 1) %>%
    str_replace_all(map_chr(treatment_headers, ~ str_c("\\\\begin{tabular}[b]{@{}c@{}}", .x, "\\\\end{tabular}")))
  tex <- str_c(header, str_sub(tex, header_end))
  tex <- if (str_detect(tex, group_heading)) {
    str_replace_all(tex, group_heading, "\\\\midrule\n\\\\multicolumn{\\1}{l}{\\\\textbf{\\2}} \\\\\\\\\n")
  } else {
    str_replace(tex, "\n(Observations &)", "\n\\\\midrule\n\\1")
  }
  write_with_retry(function() write_lines(tex, file))
}

### ----- Regression covariates and standard errors -----

# Recode covariates for the regressions: non-binary participants are too few to estimate a coefficient for, so their
# gender is set to missing (which drops them from the specifications with covariates), and the one participant without
# a high school diploma is merged with the high school graduates into "No college". College degree or above is the
# omitted education category

prepare_regression_covariates <- function(data) {
  data %>%
    mutate(gender = factor(if_else(gender == "Non-binary", NA_character_, as.character(gender)),
                           levels = setdiff(levels(gender), "Non-binary")),
           edu = fct_collapse(edu, "No college" = c("Below high school diploma", "High school diploma")),
           edu = fct_relevel(edu, "College degree or above"))
}

# Robust standard errors for all regressions: HC3 (lm_robust's se_type for OLS, sandwich::vcovHC for logits)

robust_se_type <- "HC3"

robust_vcov <- function(model) {
  if (inherits(model, "glm")) vcovHC(model, type = robust_se_type) else vcov(model)
}

### ----- Regression tables (texreg) -----

# Row labels for regression coefficients, in display order, for models estimated on data (after any releveling)

regression_coef_map <- function(data) {
  level_coefs <- function(var) {
    lvls <- levels(data[[var]])[-1]
    display <- covariate_level_names[[var]] %||% character()
    set_names(latex_dollars(if_else(lvls %in% names(display), unname(display[lvls]), lvls)), str_c(var, lvls))
  }
  c(set_names(treatment_names[-1], str_c("treatment", names(treatment_names)[-1])),
    "criterionstrict" = "Strict criterion",
    "lambda_i_adj" = "Loss aversion",
    "treatment2_lambda_i_adj" = str_c(treatment_names[["2"]], " $\\times$ Loss aversion"),
    "demo_sets_attempted" = "Baseline sets attempted", "demo_sets_completed" = "Baseline sets completed",
    "demo_mistakes" = "Baseline mistakes",
    level_coefs("gender"), level_coefs("race"), level_coefs("income"), level_coefs("edu"),
    "econYes" = "Studied economics",
    level_coefs("occupation"),
    "mouseYes" = "Used mouse",
    "(Intercept)" = "Constant")
}

# Blocks of rows shown under a bold heading in the full regression tables (baseline performance has no heading, so that
# the longest tables fit on a page)

regression_groups <- function(data) {
  list("\\textbf{Gender}" = str_c("gender", levels(data$gender)[-1]),
       "\\textbf{Race}" = str_c("race", levels(data$race)[-1]),
       "\\textbf{Household income}" = str_c("income", levels(data$income)[-1]),
       "\\textbf{Education level}" = str_c("edu", levels(data$edu)[-1]),
       "\\textbf{Occupation}" = str_c("occupation", levels(data$occupation)[-1]))
}

# Describe a list of items in prose ("a, b and c")

prose_list <- function(items) {
  if (length(items) == 1) items else str_c(str_c(head(items, -1), collapse = ", "), " and ", last(items))
}

# Table note for a regression table. type is "main" (treatment effects only), "full" (all coefficients) or
# "la" (loss aversion interaction); covariates are the covariates in the specifications that control for them

regression_note <- function(models, data, type, covariates, outcome_note = NULL) {
  covariate_prose <- c(gender = "gender", race = "race", income = "household income", edu = "education level",
                       econ = "economics studies", occupation = "occupation", mouse = "mouse use")
  estimator <- if (inherits(models[[1]], "glm")) {
    str_c("Logit estimates, with robust (", robust_se_type, ") standard errors in parentheses.")
  } else {
    str_c("OLS estimates, with heteroskedasticity-robust (", models[[1]]$se_type, ") standard errors in parentheses.")
  }
  controls <- switch(type,
    main = str_c("All specifications control for whether the strict criterion was applied. Baselines are sets attempted, ",
                 "sets completed and mistakes in the demo task; covariates are ", prose_list(covariate_prose[covariates]),
                 ". Specifications with covariates use the participants who answered all covariate questions, except the ",
                 "few non-binary participants."),
    full = {
      factors <- intersect(c("gender", "race", "income", "edu", "occupation"), covariates)
      omitted <- map_chr(factors, function(var) {
        ref <- levels(data[[var]])[1]
        display <- covariate_level_names[[var]]
        latex_dollars(if (ref %in% names(display)) display[[ref]] else ref)
      })
      str_c("Omitted categories: ", prose_list(c(treatment_names[["1"]], "lenient criterion", omitted)),
            ". Baseline variables are from the demo task. Columns (2) and (4) exclude participants with missing ",
            "covariates and the few non-binary participants.")
    },
    la = str_c("Loss aversion is the participant's estimated $\\lambda_i$, top-coded at 4.33 and bottom-coded at 0. ",
               "Both specifications control for baseline performance in the demo task (sets attempted, sets completed ",
               "and mistakes); column (2) also controls for ", prose_list(covariate_prose[covariates]),
               ". The sample is the participants whose loss aversion could be estimated and, in column (2), who answered ",
               "all covariate questions, except the few non-binary participants."))
  nan_columns <- which(map_lgl(models, ~ any(is.nan(sqrt(diag(robust_vcov(.x)))))))
  nan_note <- if (length(nan_columns) > 0) {
    str_c("Standard errors in column", if (length(nan_columns) > 1) "s" else "", " ",
          prose_list(str_c("(", nan_columns, ")")), " cannot be computed.")
  }
  omitted_group <- if (type != "full") str_c("The omitted group is ", treatment_names[["1"]], ".") #full: listed in controls
  str_c(c(estimator, outcome_note, omitted_group, controls, nan_note, star_note), collapse = " ")
}

# Save a regression table as LaTeX: columns numbered (1), (2), ..., labelled rows, the number of observations as the
# only fit statistic, and the note set below the table at the table's width. coef_map lists the rows to show, in order;
# control_rows flags which controls each column includes; note_size is the font size of the note (empty to keep the
# surrounding size)

save_regression_tex <- function(models, file, outcome, coef_map, note, groups = list(), control_rows = NULL,
                                note_size = "\\footnotesize") {
  present <- unique(unlist(map(models, ~ names(coef(.x)))))
  coef_map <- coef_map[names(coef_map) %in% present]
  group_rows <- groups %>%
    map(~ which(names(coef_map) %in% .x)) %>%
    discard(~ length(.x) == 0)
  # Logits: replace texreg's default standard errors and p-values with robust ones (lm_robust models already have them)
  robust_tests <- if (inherits(models[[1]], "glm")) map(models, ~ coeftest(.x, vcov. = robust_vcov(.x)))
  tabular <- texreg(models, digits = 2, stars = star_levels, include.ci = FALSE,
                    override.se = if (is.null(robust_tests)) 0 else map(robust_tests, ~ .x[, 2]),
                    override.pvalues = if (is.null(robust_tests)) 0 else map(robust_tests, ~ .x[, 4]),
                    custom.header = set_names(list(seq_along(models)), outcome),
                    custom.model.names = str_c("(", seq_along(models), ")"),
                    custom.coef.map = as.list(coef_map),
                    groups = if (length(group_rows) > 0) group_rows else NULL,
                    custom.gof.rows = control_rows,
                    include.aic = FALSE, include.bic = FALSE, include.loglik = FALSE, include.deviance = FALSE,
                    include.rsquared = FALSE, include.adjrs = FALSE, include.rmse = FALSE,
                    booktabs = TRUE, use.packages = FALSE, custom.note = "", table = FALSE) %>%
    as.character() %>%
    str_remove_all("(?m)^[ \\t]*(&[ \\t]*)+\\\\\\\\[ \\t]*\\n") %>% #empty rows that texreg adds below group headings
    str_replace_all(fixed("$$"), "") %>% #standard errors that cannot be computed
    str_replace(fixed("Num. obs."), "Observations") %>%
    str_trim()
  tex <- c("% Generated by the R scripts in code/; the note is set at the width of the table",
           "\\ifdefined\\regtablebox\\else\\newsavebox{\\regtablebox}\\fi",
           "\\sbox{\\regtablebox}{%",
           tabular,
           "}%",
           "\\begin{minipage}{\\wd\\regtablebox}",
           "\\usebox{\\regtablebox}",
           "",
           "\\smallskip",
           str_c("{", note_size, "\\raggedright\\textit{Notes:} ", note, "\\par}"),
           "\\end{minipage}")
  write_with_retry(function() write_lines(tex, file))
}

## ----- Figures -----

# Save a ggplot as PDF with the same dimensions as the quarto manuscript (fig-width 6, fig-asp 0.618)

save_figure_pdf <- function(plot, file) {
  write_with_retry(function() ggsave(file, plot, width = 6, height = 6 * 0.618, device = "pdf"))
}

## ----- Numbers cited in the text -----

# Write named values as LaTeX macros (e.g. list(NumObs = 642) becomes \newcommand{\NumObs}{642})

write_tex_macros <- function(values, file) {
  tex <- c("% Generated by the R scripts in code/; do not edit by hand",
           sprintf("\\newcommand{\\%s}{%s}", names(values), unlist(values)))
  write_with_retry(function() write_lines(tex, file))
}

# Format a p-value as cited in the text: two significant figures, or "unestimable" if it could not be computed

format_p_value <- function(p) {
  if_else(is.na(p), "unestimable", trimws(formatC(signif(p, 2), format = "fg", digits = 2, flag = "#")))
}

# Turn a test label into a letters-only macro name (LaTeX macro names cannot contain digits or spaces),
# e.g. "Sets completed: 1 vs 2" becomes "SetsCompletedOneVsTwo"

macro_label <- function(label) {
  label %>%
    str_replace_all(c("1" = " one ", "2" = " two ", "3" = " three ", "4" = " four ")) %>%
    str_to_title() %>%
    str_remove_all("[^A-Za-z]")
}

# Name each test's p-value as a macro: prefix + test label + "Pval" (e.g. DistTestPooledSetsCompletedOneVsTwoPval)

p_value_macros <- function(labels, p_values, prefix) {
  set_names(as.list(format_p_value(p_values)), str_c(prefix, macro_label(labels), "Pval"))
}

## ----- Hypothesis tests -----

# Tidy a named list of hypothesis tests into one table of test statistics and p-values

tidy_tests <- function(tests) {
  tests %>%
    map(~ tibble(statistic = unname(.x$statistic), p.value = .x$p.value, method = .x$method)) %>%
    bind_rows(.id = "test")
}

# P-value of a linear hypothesis test on regression coefficients, using the same robust standard errors as the tables
# (NA if it cannot be computed, e.g. NaN standard errors)

lh_p_value <- function(model, hypothesis) {
  tryCatch(linearHypothesis(model, hypothesis, vcov. = robust_vcov(model))[2, "Pr(>Chisq)"], error = function(e) NA_real_)
}
