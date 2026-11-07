# Clean the raw Qualtrics data from the lab, microeconomics class and macroeconomics class samples
# Outputs: data/slider_*_data_clean.csv (readable copies) and data/*.rds (keeping factor levels, read by the analysis scripts)

# ----- Setup -----

# Load packages and helpers, unless already done by code/run_all.R

if (!exists("run_all")) source("code/setup.R")

# ----- Read in data -----

slider_lab_data_raw <- read_csv("data/slider_lab_data_raw.csv")

slider_micro_data_raw <- read_csv("data/slider_micro_data_raw.csv")

slider_macro_data_raw <- read_csv("data/slider_macro_data_raw.csv")

# ----- Tidy and merge samples -----

## ----- Remove extra whitespaces -----

slider_lab_data <- slider_lab_data_raw %>%
  rename_with(str_squish) %>%
  mutate(across(where(is.character), str_squish))

slider_micro_data <- slider_micro_data_raw %>%
  rename_with(str_squish) %>%
  mutate(across(where(is.character), str_squish))

slider_macro_data <- slider_macro_data_raw %>%
  rename_with(str_squish) %>%
  mutate(across(where(is.character), str_squish))

## ----- Rename column headers -----

slider_lab_data <- slider_lab_data %>%
  rename(gender = Q9.1,
         race = Q9.2,
         race_other = Q9.2_7_TEXT,
         age = Q9.3,
         income = Q9.4,
         edu = Q9.5,
         econ = Q9.6,
         occupation = Q9.7,
         occupation_other = Q9.7_8_TEXT,
         mouse = Q9.8,
         targets_achieve_attempt = Q9.9,
         sets_completed_goal = Q9.10,
         mistakes_goal = Q9.11,
         targets_ignore_reason = Q9.12,
         targets_ignore_reason_other = Q9.12_4_TEXT,
         targets_explanation_help = Q9.13,
         fix_5 = Q10.1_1,
         fix_7.5 = Q10.1_3,
         fix_10 = Q10.1_4,
         fix_12.5 = Q10.1_5,
         fix_15 = Q10.1_6,
         fix_17.5 = Q10.1_2,
         fix_20 = Q10.1_7,
         random_5 = Q11.1_1,
         random_7.5 = Q11.1_3,
         random_10 = Q11.1_4,
         random_12.5 = Q11.1_5,
         random_15 = Q11.1_6,
         random_17.5 = Q11.1_2,
         random_20 = Q11.1_9,
         common_instructions_time = `Q2.1_Page Submit...42`,
         treatment1_instructions_time = `Q1.1_Page Submit`,
         treatment2_instructions_time = `Q2.1_Page Submit...44`,
         treatment3_instructions_time = `Q3.1_Page Submit`,
         treatment4_instructions_time = `Q4.1_Page Submit`) %>%
  rename_with(str_to_lower)

slider_micro_data <- slider_micro_data %>%
  rename(gender = Q5.1,
         race = Q5.2,
         race_other = Q5.2_7_TEXT,
         age = Q5.3,
         income = Q5.4,
         mouse = Q5.7,
         targets_achieve_attempt = Q5.8,
         sets_completed_goal = Q5.9,
         mistakes_goal = Q5.10,
         targets_ignore_reason = Q5.11,
         targets_ignore_reason_other = Q5.11_4_TEXT,
         targets_explanation_help = Q5.12,
         fix_5 = Q6.1_1,
         fix_7.5 = Q6.1_3,
         fix_10 = Q6.1_4,
         fix_12.5 = Q6.1_5,
         fix_15 = Q6.1_6,
         fix_17.5 = Q6.1_2,
         fix_20 = Q6.1_7,
         random_5 = Q7.1_1,
         random_7.5 = Q7.1_3,
         random_10 = Q7.1_4,
         random_12.5 = Q7.1_5,
         random_15 = Q7.1_6,
         random_17.5 = Q7.1_2,
         random_20 = Q7.1_9,
         common_instructions_time = `Q2.1_Page Submit`,
         treatment1_instructions_time = `Q262_Page Submit`,
         treatment2_instructions_time = `Q21.1_Page Submit`,
         treatment3_instructions_time = `Q22.1_Page Submit`,
         treatment4_instructions_time = `Q23.1_Page Submit`) %>%
  rename_with(str_to_lower)

slider_macro_data <- slider_macro_data %>%
  rename(gender = Q5.1,
         race = Q5.2,
         race_other = Q5.2_7_TEXT,
         age = Q5.3,
         income = Q5.4,
         mouse = Q5.7,
         targets_achieve_attempt = Q5.8,
         sets_completed_goal = Q5.9,
         mistakes_goal = Q5.10,
         targets_ignore_reason = Q5.11,
         targets_ignore_reason_other = Q5.11_4_TEXT,
         targets_explanation_help = Q5.12,
         fix_5 = Q6.1_1,
         fix_7.5 = Q6.1_3,
         fix_10 = Q6.1_4,
         fix_12.5 = Q6.1_5,
         fix_15 = Q6.1_6,
         fix_17.5 = Q6.1_2,
         fix_20 = Q6.1_7,
         random_5 = Q7.1_1,
         random_7.5 = Q7.1_3,
         random_10 = Q7.1_4,
         random_12.5 = Q7.1_5,
         random_15 = Q7.1_6,
         random_17.5 = Q7.1_2,
         random_20 = Q7.1_9,
         common_instructions_time = `Q2.1_Page Submit`,
         treatment1_instructions_time = `Q262_Page Submit`,
         treatment2_instructions_time = `Q21.1_Page Submit`,
         treatment3_instructions_time = `Q22.1_Page Submit`,
         treatment4_instructions_time = `Q23.1_Page Submit`) %>%
  rename_with(str_to_lower)

## ----- Remove non-observations -----

# Remove first two rows (non-observations) from each dataset

slider_lab_data <- slider_lab_data[-1:-2, ]

slider_micro_data <- slider_micro_data[-1:-2, ]

slider_macro_data <- slider_macro_data[-1:-2, ]

# Retain only actual study participants by filtering on the varses id in the lab dataset

slider_lab_data <- slider_lab_data %>%
  filter(var_ses != "test" & var_ses != 79061) %>%
  select(-var_ses)

## ----- Merge samples -----

# Create variable to distinguish each dataset by their samples before merging

slider_lab_data <- slider_lab_data %>%
  mutate(sample = "lab")

slider_micro_data <- slider_micro_data %>%
  mutate(sample = "micro")

slider_macro_data <- slider_macro_data %>%
  mutate(sample = "macro")

# Merge the datasets row-wise

slider_data_full <- bind_rows(slider_lab_data, slider_micro_data, slider_macro_data)

# Remove those who did not finish

slider_data <- slider_data_full %>%
  filter(finished == "True") %>%
  select(-finished)

# ----- Convert data types -----

# Inspect data structure and convert to desired data types

str(slider_data)

## ----- Convert to factors -----

slider_data <- slider_data %>%
  mutate(recordeddate = as.POSIXct(recordeddate, format = "%Y-%m-%d %H:%M:%S"),
         treatment = factor(treatment),
         criterion = factor(criterion),
         gender = factor(gender) %>%
           relevel(ref = "Male"),
         race = factor(race),
         race_other = factor(race_other),
         income = factor(income, levels = c("0 - 24,999", "25,000 - 49,999", "50,000 - 74,999",
                                            "75,000 - 119,999", "120,000 - 199,999", "200,000 and over")),
         edu = factor(edu),
         econ = factor(econ, levels = c("No", "Yes")),
         occupation = factor(occupation),
         occupation_other = factor(occupation_other),
         mouse = factor(mouse, levels = c("No","Yes")),
         targets_achieve_attempt = factor(targets_achieve_attempt, levels = c("No for both targets",
                                                                              "Yes but only for accuracy target",
                                                                              "Yes but only for speed target",
                                                                              "Yes for both targets.")),
         targets_ignore_reason = factor(targets_ignore_reason, levels = c("Did not care about target(s)",
                                                                          "Too difficult to understand/ recall target(s)",
                                                                          "Too difficult to achieve target(s)",
                                                                          "Other")),
         targets_ignore_reason_other = factor(targets_ignore_reason_other),
         targets_explanation_help = factor(targets_explanation_help, levels = c("No", "Yes")),
         sample = factor(sample))

## ----- Convert to numeric -----

# Convert to numerical only for variables which either have numerical values or NA

slider_data <- slider_data %>%
  mutate(across(where(is.character), ~ {
    converted <- suppressWarnings(as.numeric(.))
    if (all(is.na(.) | !is.na(converted))) converted else .
  }))

str(slider_data)

# ----- Fix numeric variables with non-numeric responses -----

# Check why some variables which are supposed to be numeric have non-numeric values and rectify them

## ----- Age -----

unique(slider_data$age)

slider_data <- slider_data %>%
  mutate(age = str_extract(age, "\\b(19|20)\\d{2}\\b"))

slider_data$age <- as.numeric(slider_data$age)

## ----- Speed and accuracy goals -----

unique(slider_data$sets_completed_goal)

unique(slider_data$mistakes_goal)

#Responses are a mix of numbers and text so use rule of thumb approach:
#If the response only contains one number, extract that
#If the response contains multiple numbers (max is 2), then we treat on a case by case basis

### ----- Speed goal -----

# Extract first number from all responses

sets_goal_firstnumber <- as.numeric(str_extract(slider_data$sets_completed_goal, "\\d+"))

# Extract responses with two numbers

sets_goal_multinumber <- as_tibble(str_match(slider_data$sets_completed_goal, "(\\d+)\\D+(\\d+)"))

sets_goal_multinumber <- sets_goal_multinumber %>%
  mutate(V2 = as.numeric(V2),
         V3 = as.numeric(V3),
         sets_goal_numeric = case_when(
    V1 == "0.5" ~ NA,
    V1 == "23, just over half of the 45" ~ 23,
    V1 == "30 because after the first few I realized I was being too slow to get to 45" ~ 30,
    V1 == "4 in the warmup, so about 20" ~ 20,
    V1 == "45, but based on the practice round I did not expect to reach that many in 5" ~ 45,
    TRUE ~ (V2 + V3)/2
  ))

# For responses with two numbers, replace the first number extracted with the value derived from the two number response

sets_goal_numeric <- coalesce(sets_goal_multinumber$sets_goal_numeric, sets_goal_firstnumber)

# Replace sets_completed_goal values in the original datset

slider_data$sets_completed_goal <- sets_goal_numeric

### ----- Mistakes goal -----

# Now repeat the same process for mistakes_goals

mistakes_goal_firstnumber <- as.numeric(str_extract(slider_data$mistakes_goal, "\\d+"))

mistakes_goal_multinumber <- as_tibble(str_match(slider_data$mistakes_goal, "(\\d+)\\D+(\\d+)"))

mistakes_goal_multinumber <- mistakes_goal_multinumber %>%
  mutate(V2 = as.numeric(V2),
         V3 = as.numeric(V3),
         mistakes_goal_numeric = case_when(
           V1 == "0 mistakes but when I saw the time ticking down and I hadn't reached 45" ~ 0,
           V1 == "1 mistake per 10" ~ 4.5,
           V1 == "2 of them to 100" ~ 2,
           V1 == "45 sets during the 5" ~ 0,
           TRUE ~ (V2 + V3)/2
         ))

mistakes_goal_numeric <- coalesce(mistakes_goal_multinumber$mistakes_goal_numeric, mistakes_goal_firstnumber)

slider_data$mistakes_goal <- mistakes_goal_numeric

## ----- Loss aversion survey -----

# Number of correctly completed sets one is willing to do under different piece rates

### ----- Fixed payment -----

unique(slider_data$fix_20)
#Same issues of ~, %, some ranges, and "none" in responses for fixed payments, so can amend them together

slider_data <- slider_data %>%
  mutate(across(fix_5:fix_20, ~ {
    new <- case_when(
    .x == "50-75" ~ "62.5",
    .x == "40-50" ~ "45",
    TRUE ~ .x
    )
  new <- str_replace_all(new, "[~%]", "")
  new <- str_replace_all(new, "(?i)^none$", "0")
  new
  }))

slider_data <- slider_data %>%
  mutate(across(fix_5:fix_20, ~ as.numeric(.)))

### ----- Randomised payment -----

unique(slider_data$random_20)
#Responses with more than one number mostly misinterpret the question; e.g. they specify effort ex post under each piece rate realisation instead of ex ante, or they choose the more preferred piece rate (which is kind of no shit), so they are invalidated.
#Only those which provide ranges are admitted and the average is taken same as before.
#Responses with one number are all accepted.

slider_data <- slider_data %>%
  mutate(across(random_5:random_20, ~ {
    new <- case_when(
      .x %in% c("No", "None", "o", "00") ~ "0",
      .x == "100-200" ~ "150",
      .x == "30-40" ~ "35",
      .x == "40-50" ~ "45",
      TRUE ~ .x
    )
    new <- if_else(str_count(new, "\\d+") == 1, str_extract(new, "\\d+(\\.\\d+)?"), NA) #extract first number only for responses which contain a single number
    new
  }))

slider_data <- slider_data %>%
  mutate(across(random_5:random_20, ~ as.numeric(.)))

str(slider_data)

# ----- Recategorise covariates -----

## ----- Race -----

# Recategorise responses for race_other and merge into race

unique(slider_data$race)
unique(slider_data$race_other)

slider_data <- slider_data %>%
  mutate(race_other_grouped = case_when(
    race_other %in% c("South East Asian", "Indian", "central asian and south asian") ~ "Asian",
    race_other %in% c("Middle Eastern", "Middle Eastern (Arab)", "Arab") ~ "White",
    race_other %in% c("Latina") ~ "Hispanic",
    T ~ "Other"
  ))

slider_data <- slider_data %>%
  mutate(race = if_else(race == "Other", race_other_grouped, race),
         race = factor(race) %>%
           relevel(ref = "White"))

levels(slider_data$race)

## ----- Age -----

# Convert age from year of birth to years old

slider_data <- slider_data %>%
  mutate(age = 2025 - age)

str(slider_data$age)

## ----- Education -----

# Rename responses for edu

unique(slider_data$edu)

slider_data <- slider_data %>%
  mutate(edu = if_else(edu == "College/ university degree or equivalent or above",
                       "College degree or above",
                       edu))

## ----- Occupation -----

# Rename responses for occupation, recategorise responses for occupation_other and merge

unique(slider_data$occupation)
unique(slider_data$occupation_other)

slider_data <- slider_data %>%
  mutate(occupation = case_when(
    occupation == "Professional services (e.g. accounting, banking, consulting)" ~ "Professional services",
    occupation == "High-tech manufacturing and engineering" ~ "High-tech mfg or eng",
    TRUE ~ occupation))

slider_data <- slider_data %>%
  mutate(occupation_other_grouped = case_when(
    occupation_other %in% c("Clerk", "Appointment setter") ~ "Clerical",
    occupation_other %in% c("Legal", "accounting", "Medicine", "auction", "research") ~ "Professional services",
    occupation_other %in% c("Retired", "Retired Health Professional", "retired") ~ "Unemployed",
    T ~ "Other"
  ))

slider_data <- slider_data %>%
  mutate(occupation = if_else(occupation == "Other", occupation_other_grouped, occupation))

levels(slider_data$occupation)

## ----- Class sample covariates -----

# Fill in education, econ, and occupation for class sample

slider_data <- slider_data %>%
  mutate(edu = if_else(sample %in% c("micro", "macro"), "College degree or above", edu),
         edu = factor(edu, levels = c("Below high school diploma", "High school diploma",
                                      "College degree or above")),
         econ = if_else(sample %in% c("micro", "macro"), "Yes", econ),
         econ = factor(econ, levels = c("No", "Yes")),
         occupation = if_else(sample %in% c("micro", "macro"), "Student", occupation),
         occupation = factor(occupation, levels = c("Student", "Academia", "Agricultural", "Clerical",
                                                    "High-tech mfg or eng",
                                                    "Managerial", "Professional services",
                                                    "Unemployed", "Other")))

levels(slider_data$occupation)

## ----- Reasons for ignoring targets -----

# Summarise responses for targets_ignore_reason_other and merge into targets_ignore_reason

unique(slider_data$targets_ignore_reason_other)
#Inherent prioritisation of accuracy over speed, greater perceived control over accuracy than speed, risk aversion towards getting the strict vs lenient criterion, forgot about/ constrained by time

# ----- Create performance variables -----

# Create variable for completion proportion

slider_data <- slider_data %>%
  mutate(completion_prop = sets_completed/ sets_attempted)

# Create variable for completion speed

slider_data <- slider_data %>%
  mutate(completion_rate = sets_completed/ 5)

# Create variable for mistake rate

slider_data <- slider_data %>%
  mutate(mistake_rate = mistakes/ sets_attempted * 100)

# Create variable for recorded mistake rate

slider_data <- slider_data %>%
  mutate(recorded_mistake_rate = if_else(criterion == "lenient", mistake_rate/ 4, mistake_rate))

# ----- Subset data by sample -----

slider_lab_data <- slider_data %>%
  filter(sample == "lab")

slider_micro_data <- slider_data %>%
  filter(sample == "micro")

slider_macro_data <- slider_data %>%
  filter(sample == "macro")

# Merge class samples

slider_class_data <- rbind(slider_micro_data, slider_macro_data)

# ----- Write out cleaned datasets -----

## ----- CSV -----

write_csv(slider_data, "data/slider_full_data_clean.csv")

write_csv(slider_lab_data, "data/slider_lab_data_clean.csv")

write_csv(slider_micro_data, "data/slider_micro_data_clean.csv")

write_csv(slider_macro_data, "data/slider_macro_data_clean.csv")

## ----- RDS -----

# RDS keeps the factor levels and reference categories set above, so the analysis scripts read these

write_rds(slider_data, "data/slider_full_data_clean.rds")

write_rds(slider_lab_data, "data/slider_lab_data_clean.rds")

write_rds(slider_micro_data, "data/slider_micro_data_clean.rds")

write_rds(slider_macro_data, "data/slider_macro_data_clean.rds")

write_rds(slider_class_data, "data/slider_class_data_clean.rds")

write_rds(slider_data_full, "data/slider_all_respondents_data.rds") #including those who did not finish
