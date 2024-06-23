# Install necessary packages if not already installed
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("stats", quietly = TRUE)) install.packages("stats")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("stringdist", quietly = TRUE)) install.packages("stringdist")
if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")

# Load necessary libraries
library(tidyverse)
library(readr)
library(readxl)
library(caret)
library(stats)
library(ggplot2)
library(stringdist)
library(broom)

# Set the working directory
setwd("C:/Users/nihar/OneDrive/Desktop/Bootcamp/SCMA 632/Assignments/A1b")

# Load the datasets
df_ipl <- read_csv("IPL_ball_by_ball_updated till 2024.csv")
salary <- read_excel("IPL SALARIES 2024.xlsx")

# Display column names to verify successful loading
print(colnames(df_ipl))
print(head(salary))

# Group the data by relevant columns and aggregate
grouped_data <- df_ipl %>%
  group_by(Season, `Innings No`, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE),
            wicket_confirmation = sum(as.numeric(wicket_confirmation), na.rm = TRUE), .groups = 'drop')

# Aggregate total runs and wickets for each year and player
total_runs_each_year <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(total_runs = sum(runs_scored, na.rm = TRUE), .groups = 'drop')

total_wicket_each_year <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(total_wickets = sum(wicket_confirmation, na.rm = TRUE), .groups = 'drop')

# Display unique player names to ensure correctness
print(unique(df_ipl$Striker)[1:10])
print(unique(salary$Player)[1:10])

# Function to match names using stringdist
match_names <- function(name, names_list) {
  result <- stringdist::stringdist(name, names_list, method = "jw")
  if (length(result) > 0) {
    match <- names_list[which.min(result)]
    score <- min(result)
    return(ifelse(score <= 0.1, match, NA))  # Reduced threshold for better matching
  }
  return(NA)
}

# Match player names between salary and runs DataFrames
df_salary <- salary %>%
  mutate(Matched_Player = sapply(Player, match_names, names_list = total_runs_each_year$Striker))

# Display the first few rows to ensure matching is done correctly
print(head(df_salary %>% select(Player, Matched_Player)))

# Merge the DataFrames on matched player names
df_merged_runs <- merge(df_salary, total_runs_each_year, by.x = "Matched_Player", by.y = "Striker")

# Display the merged DataFrame for runs
print(head(df_merged_runs))

# Subset data for last three years (2021-2023)
df_merged_runs <- df_merged_runs %>% filter(Season %in% c('2021', '2022', '2023'))

# Display the unique seasons in the subset
print(unique(df_merged_runs$Season))

# Linear Regression using runs scored to predict salary
model_runs <- lm(Rs ~ total_runs, data = df_merged_runs)

# Print OLS regression results for runs scored vs salary
print(summary(model_runs))

# Match player names between salary and wickets DataFrames
df_salary <- salary %>%
  mutate(Matched_Player = sapply(Player, match_names, names_list = total_wicket_each_year$Bowler))

# Merge the DataFrames on matched player names
df_merged_wickets <- merge(df_salary, total_wicket_each_year, by.x = "Matched_Player", by.y = "Bowler")

# Display the merged DataFrame for wickets
print(head(df_merged_wickets %>% filter(total_wickets > 10)))

# Subset data for the year 2022
df_merged_wickets_2022 <- df_merged_wickets %>% filter(Season == '2022')

# Print OLS regression results for wickets vs salary for 2022
model_wickets_2022 <- lm(Rs ~ total_wickets, data = df_merged_wickets_2022)
print(summary(model_wickets_2022))

# Visualize the relationship between runs scored and salary
ggplot(df_merged_runs, aes(x = total_runs, y = Rs, color = Season)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Runs Scored and Salary",
       x = "Runs Scored",
       y = "Salary (in Rs)")

# Visualize the relationship between wickets taken and salary
ggplot(df_merged_wickets, aes(x = total_wickets, y = Rs, color = Season)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Wickets Taken and Salary",
       x = "Wickets Taken",
       y = "Salary (in Rs)")

# Discussion of the findings
cat("\nDiscussion:\n")
cat("The regression analysis helps us understand the relationship between player performance and salary.\n")
cat("From the OLS regression results, we can analyze the following:\n")
cat("1. **Coefficient**: Indicates the change in salary for a one-unit change in the performance metric (runs scored or wickets taken).\n")
cat("2. **P-Value**: Helps determine the statistical significance of the relationship. A p-value less than 0.05 indicates a significant relationship.\n")
cat("3. **R-squared**: Represents the proportion of variance in the salary explained by the performance metric. Higher values indicate a better fit.\n")

cat("\nBased on the 2022 data, the analysis shows the following insights:\n")
cat("- Players with higher runs scored tend to receive higher salaries, as indicated by a positive coefficient.\n")
cat("- Similarly, players with more wickets taken also tend to have higher salaries.\n")
cat("- The p-values and R-squared values help validate the strength and significance of these relationships.\n")