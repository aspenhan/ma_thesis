# Make an id variable for each column

slider_data <- slider_data %>%
  mutate(id = row_number())

# Subset loss aversion data

loss_aversion_data <- slider_data %>%
  select(id, fix_5:random_20)

# Create one row for each payment structure for each subject and create variables for regression

loss_aversion_data <- loss_aversion_data %>%
  pivot_longer(cols = -id, names_to = "payment_type", values_to = "effort")

loss_aversion_data <- loss_aversion_data %>%
  mutate(w_mean = as.numeric(str_extract(payment_type, "\\d+")))

loss_aversion_data <- loss_aversion_data %>%
  mutate(w_spread = case_when(
    str_detect(payment_type, "fix") ~ 0,
    payment_type == "random_5" ~ 6,
    payment_type == "random_7.5" ~ 7,
    payment_type == "random_10" ~ 10,
    payment_type == "random_12.5" ~ 13,
    payment_type == "random_15" ~ 14,
    payment_type == "random_17.5" ~ 17,
    payment_type == "random_20" ~ 20
  ))

loss_aversion_data <- loss_aversion_data %>%
  mutate(effort = if_else(effort == 0, 1, effort))

loss_aversion_data <- loss_aversion_data %>%
  mutate(log_effort = log(effort),
         log_wage_mean = log(w_mean),
         wage_ratio = w_spread / w_mean) %>%
  filter(!is.na(effort))

# Estimate loss aversion reduced form 

loss_aversion_reduced_form <- loss_aversion_data %>%
  group_by(id) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(log_effort ~ log_wage_mean + wage_ratio, data = .x)),
         tidied = map(model, tidy)) %>%
  unnest(tidied)

reduced_form_estimates <- loss_aversion_reduced_form %>%
  select(id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  rename("J_i" = "(Intercept)", "K_i" = "log_wage_mean", "L_i" = "wage_ratio")

loss_aversion_data <- loss_aversion_data %>%
  left_join(reduced_form_estimates, by = "id")

# Calculate structural estimates

loss_aversion_data <- loss_aversion_data %>%
  mutate(L_i = -1*L_i)
  
loss_aversion_data <- loss_aversion_data %>%
  mutate(lambda_i = 1 + (4 * L_i / K_i),
         lambda_i_adj = case_when(
           lambda_i > 4.33 ~ 4.33,
           lambda_i < 0 ~ 0,
           TRUE ~ lambda_i
         ))

# Merge back into slider data

slider_data <- slider_data %>%
  left_join(loss_aversion_data %>% select(id, J_i: lambda_i_adj), by = "id") %>%
  distinct(id, .keep_all = TRUE)

