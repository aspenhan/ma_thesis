# Sample descriptive figures: trade-offs between speed and accuracy in the slider task
# Outputs: output/figures/effort_tradeoffs_*.pdf

# ----- Setup -----

# Load packages and helpers, unless already done by code/run_all.R

if (!exists("run_all")) source("code/setup.R")

# ----- Load data -----

slider_data <- read_rds("data/slider_full_data_clean.rds")

slider_lab_data <- read_rds("data/slider_lab_data_clean.rds")

slider_micro_data <- read_rds("data/slider_micro_data_clean.rds")

slider_macro_data <- read_rds("data/slider_macro_data_clean.rds")

# ----- Speed and accuracy trade-offs (pooled sample) -----

## ----- All data -----

# Effort combination counts

effort_comb_count <- slider_data %>%
  count(sets_completed, mistake_rate)

# Scatter plot and fitted line of speed and accuracy metrics

effort_tradeoff_plot <- ggplot() +
  geom_point(data = effort_comb_count, aes(x = sets_completed, y = mistake_rate, size = n)) +
  geom_smooth(data = slider_data, aes(x = sets_completed, y = mistake_rate),
              method = "loess", linewidth = 1) +
  labs(x = "Sets completed", y = "Mistake rate (%)", size = "Counts") +
  def_theme

save_figure_pdf(effort_tradeoff_plot, "output/figures/effort_tradeoffs_all.pdf")

## ----- Without outliers -----

slider_data_no_outliers <- slider_data %>%
  filter(between(sets_completed, quantile(sets_completed, 0.25) - 1.5 * IQR(sets_completed), quantile(sets_completed, 0.75) + 1.5 * IQR(sets_completed))) %>%
  filter(between(mistake_rate, quantile(mistake_rate, 0.25) - 1.5 * IQR(mistake_rate), quantile(mistake_rate, 0.75) + 1.5 * IQR(mistake_rate)))

effort_comb_count_no_outliers <- slider_data_no_outliers %>%
  count(sets_completed, mistake_rate)

effort_tradeoff_subset_plot <- ggplot() +
  geom_point(data = effort_comb_count_no_outliers, aes(x = sets_completed, y = mistake_rate, size = n)) +
  geom_smooth(data = slider_data_no_outliers, aes(x = sets_completed, y = mistake_rate),
              method = "loess", linewidth = 1) +
  labs(x = "Sets completed", y = "Mistake rate (%)", size = "Counts") +
  def_theme

save_figure_pdf(effort_tradeoff_subset_plot, "output/figures/effort_tradeoffs_no_outliers.pdf")

# ----- Speed and accuracy trade-offs by subsample (not shown in the manuscript) -----

## ----- Lab -----

effort_tradeoff_plot_lab <- ggplot(data = slider_lab_data, aes(x = sets_completed, y = mistake_rate)) +
  geom_point() +
  geom_smooth(method = "loess", linewidth = 1) +
  labs(x = "Sets completed", y = "Mistake rate (%)", size = "Counts") +
  def_theme

save_figure_pdf(effort_tradeoff_plot_lab, "output/figures/effort_tradeoffs_lab.pdf")

## ----- Micro -----

effort_tradeoff_plot_micro <- ggplot(data = slider_micro_data, aes(x = sets_completed, y = mistake_rate)) +
  geom_point() +
  geom_smooth(method = "loess", linewidth = 1) +
  labs(x = "Sets completed", y = "Mistake rate (%)", size = "Counts") +
  def_theme

save_figure_pdf(effort_tradeoff_plot_micro, "output/figures/effort_tradeoffs_micro.pdf")

## ----- Macro -----

effort_tradeoff_plot_macro <- ggplot(data = slider_macro_data, aes(x = sets_completed, y = mistake_rate)) +
  geom_point() +
  geom_smooth(method = "loess", linewidth = 1) +
  labs(x = "Sets completed", y = "Mistake rate (%)", size = "Counts") +
  def_theme

save_figure_pdf(effort_tradeoff_plot_macro, "output/figures/effort_tradeoffs_macro.pdf")
