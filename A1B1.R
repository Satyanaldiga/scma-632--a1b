library(bit64)
library(bit)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(readxl)
library(MASS)
library(nortest)

# Set file path
file_path <- "C:\\Users\\SPURGE\\Desktop\\SCMA\\IPL_ball_by_ball_updated till 2024.csv"
salary_file_path <- "C:\\Users\\SPURGE\\Desktop\\SCMA\\IPL_SALARIES_2024.xlsx"

# Load the IPL data
ipl_data <- read_csv(file_path)

# Display basic information about the data to understand its structure
str(ipl_data)
head(ipl_data)


# Clean the Season column to have only the highest year in the format '2007/08' -> '2008'
correct_season <- function(season) {
  if (grepl("/", season)) {
    years <- unlist(strsplit(season, "/"))
    return(paste0("20", years[2]))
  }
  return(season)
}

ipl_data$Season <- sapply(ipl_data$Season, correct_season)


# Additional correction for remaining anomalies
season_corrections <- c("8" = "2008", "10" = "2010", "21" = "2020")

ipl_data$Season <- ifelse(ipl_data$Season %in% names(season_corrections),
                          season_corrections[ipl_data$Season],
                          ipl_data$Season)

# Verify the changes
unique(ipl_data$Season)

# Data Type Conversion
# Convert 'Date' to date format
ipl_data$Date <- dmy(ipl_data$Date)

# Convert 'Season' to character
ipl_data$Season <- as.character(ipl_data$Season)

# Remove Duplicates
# Check for duplicate rows
duplicates <- duplicated(ipl_data)
cat("Number of duplicate rows:", sum(duplicates), "\n")


# Remove duplicate rows
ipl_data <- ipl_data[!duplicates, ]


# Handling Inconsistencies
# Standardize team names (example: replace inconsistent team names)
team_name_corrections <- c(
  "Delhi Daredevils" = "Delhi Capitals",
  "Rising Pune Supergiant" = "Rising Pune Supergiants",
  "Gujarat Lions" = "Gujarat Titans",
  "Pune Warriors" = "Rising Pune Supergiants",
  "Deccan Chargers" = "Sunrisers Hyderabad",
  "Royal Challengers Bangalore" = "Royal Challengers Bengaluru",
  "Kings XI Punjab" = "Punjab Kings"
)

ipl_data$`Batting team` <- ifelse(ipl_data$`Batting team` %in% names(team_name_corrections),
                                  team_name_corrections[ipl_data$`Batting team`],
                                  ipl_data$`Batting team`)

ipl_data$`Bowling team` <- ifelse(ipl_data$`Bowling team` %in% names(team_name_corrections),
                                  team_name_corrections[ipl_data$`Bowling team`],
                                  ipl_data$`Bowling team`)

# Filtering Irrelevant Data
# Ensure no rows with missing match id or date
ipl_data <- ipl_data[!is.na(ipl_data$`Matchid`) & !is.na(ipl_data$Date), ]


# Display the cleaned data info
str(ipl_data)
head(ipl_data)


# List of unique teams in the 'Batting team' and 'Bowling team' columns
batting_teams <- unique(ipl_data$`Batting team`)
bowling_teams <- unique(ipl_data$`Bowling team`)

# Combine both lists and find unique teams
all_teams <- unique(c(batting_teams, bowling_teams))

all_teams_sorted <- sort(all_teams)
print(all_teams_sorted)


ipl_data$`Batting team` <- ifelse(ipl_data$`Batting team` %in% names(team_name_corrections),
                                  team_name_corrections[ipl_data$`Batting team`],
                                  ipl_data$`Batting team`)

ipl_data$`Bowling team` <- ifelse(ipl_data$`Bowling team` %in% names(team_name_corrections),
                                  team_name_corrections[ipl_data$`Bowling team`],
                                  ipl_data$`Bowling team`)

# Display the updated list of unique teams
batting_teams_updated <- unique(ipl_data$`Batting team`)
bowling_teams_updated <- unique(ipl_data$`Bowling team`)

all_teams_updated <- unique(c(batting_teams_updated, bowling_teams_updated))


all_teams_sorted_updated <- sort(all_teams_updated)
print(all_teams_sorted_updated)

# Display the top 30 rows of the cleaned data
top_30_rows <- head(ipl_data, 30)
print(top_30_rows)


# Group data by 'Match id' to represent each round
round_wise_data <- ipl_data %>%
  group_by(`Matchid`)


# Aggregate runs scored by each batsman per match
batsman_aggregate <- ipl_data %>%
  group_by(`Matchid`, Striker) %>%
  summarise(runs = sum(runs_scored))

# Aggregate wickets taken by each bowler per match
bowler_aggregate <- ipl_data %>%
  filter(wicket_confirmation == 1) %>%
  group_by(`Matchid`, Bowler) %>%
  summarise(wickets = sum(wicket_confirmation))

# Function to get top 3 performers in each round
top_performers <- function(df, group_col, value_col, top_n = 3) {
  df %>%
    arrange(desc(!!sym(value_col))) %>%
    group_by(!!sym(group_col)) %>%
    slice_max(order_by = !!sym(value_col), n = top_n)
}

# Get top 3 run-scorer in each match
top_run_getters <- top_performers(batsman_aggregate, 'Matchid', 'runs')

# Get top 3 wicket-takers in each match
top_wicket_takers <- top_performers(bowler_aggregate, 'Matchid', 'wickets')


# Display the results
cat("Top 3 Run-Getters in Each Match:\n")
print(top_run_getters)


cat("\nTop 3 Wicket-Takers in Each Match:\n")
print(top_wicket_takers)

# Aggregate runs scored by each batsman per season
batsman_aggregate_season <- ipl_data %>%
  group_by(Season, Striker) %>%
  summarise(runs = sum(runs_scored))


# Aggregate wickets taken by each bowler per season
bowler_aggregate_season <- ipl_data %>%
  filter(wicket_confirmation == 1) %>%
  group_by(Season, Bowler) %>%
  summarise(wickets = sum(wicket_confirmation))


# Function to get top 3 performers in each season
top_performers_season <- function(df, group_col, value_col, top_n = 3) {
  df %>%
    arrange(desc(!!sym(value_col))) %>%
    group_by(!!sym(group_col)) %>%
    slice_max(order_by = !!sym(value_col), n = top_n)
}

# Get top 3 run-scorer in each season
top_run_getters_season <- top_performers_season(batsman_aggregate_season, 'Season', 'runs')

# Get top 3 wicket-takers in each season
top_wicket_takers_season <- top_performers_season(bowler_aggregate_season, 'Season', 'wickets')

# Organize and display the results
cat("Top 3 Run-Getters in Each Season:\n")
seasons <- unique(top_run_getters_season$Season)
for (season in seasons) {
  cat("\nSeason:", season, "\n")
  season_data <- filter(top_run_getters_season, Season == season)
  for (i in 1:nrow(season_data)) {
    cat(paste("Batsman:", season_data$Striker[i], ", Runs:", season_data$runs[i], "\n"))
  }
}


cat("\nTop 3 Wicket-Takers in Each Season:\n")
for (season in seasons) {
  cat("\nSeason:", season, "\n")
  season_data <- filter(top_wicket_takers_season, Season == season)
  for (i in 1:nrow(season_data)) {
    cat(paste("Bowler:", season_data$Bowler[i], ", Wickets:", season_data$wickets[i], "\n"))
  }
}


# Ensure data is sorted by Season in descending order and by runs/wickets in descending order
top_run_getters_season <- top_run_getters_season %>%
  arrange(desc(Season), desc(runs))


top_wicket_takers_season <- top_wicket_takers_season %>%
  arrange(desc(Season), desc(wickets))


# Organize and display the results in the specified format
for (season in seasons) {
  cat("\nSeason:", season, "\n")
  
  # Top 3 Run-Getters
  cat("Top 3 Run-Getters:\n")
  season_data_batsmen <- head(filter(top_run_getters_season, Season == season), 3)
  for (i in 1:nrow(season_data_batsmen)) {
    team <- filter(ipl_data, Season == season & Striker == season_data_batsmen$Striker[i])$`Batting team`[1]
    cat(paste(i, ". Batsman:", season_data_batsmen$Striker[i], "(", team, "), Runs:", season_data_batsmen$runs[i], "\n"))
  }
  
  # Top 3 Wicket-Takers
  cat("Top 3 Wicket-Takers:\n")
  season_data_bowlers <- head(filter(top_wicket_takers_season, Season == season), 3)
  for (i in 1:nrow(season_data_bowlers)) {
    team <- filter(ipl_data, Season == season & Bowler == season_data_bowlers$Bowler[i])$`Bowling team`[1]
    cat(paste(i, ". Bowler:", season_data_bowlers$Bowler[i], "(", team, "), Wickets:", season_data_bowlers$wickets[i], "\n"))
  }
}


# Filter data for the last three IPL seasons
last_three_seasons <- c('2024', '2023', '2022')
top_batsmen_last_three_seasons <- filter(top_run_getters_season, Season %in% last_three_seasons)
top_bowlers_last_three_seasons <- filter(top_wicket_takers_season, Season %in% last_three_seasons)

# Function to remove outliers using IQR method
remove_outliers <- function(data) {
  Q1 <- quantile(data, 0.25)
  Q3 <- quantile(data, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data[data >= lower_bound & data <= upper_bound]
}

# Function to fit and plot normal distribution
fit_and_plot_normal <- function(data, title, xlabel) {
  if(length(data) == 0) {
    cat(paste("No data available for", title, "\n"))
    return(NULL)
  }
  
  mu <- mean(data)
  std <- sd(data)
  
  # Adjust plot margins
  par(mar = c(5, 4, 4, 2) + 0.1)
  
  # Resize the plotting window
  windows(width = 10, height = 6)  # For Windows
  
  # Plot histogram
  hist(data, breaks = 10, freq = FALSE, col = 'blue', main = title, xlab = xlabel, ylab = 'Density')
  
  # Plot PDF
  x <- seq(min(data), max(data), length = 100)
  lines(x, dnorm(x, mean = mu, sd = std), col = 'black', lwd = 2)
  
  cat(paste(title, "\nFit results: mu =", round(mu, 2), ", std =", round(std, 2), "\n"))
}


# Remove outliers and fit normal distribution for runs scored by top batsmen in the last three seasons
runs_data <- remove_outliers(top_batsmen_last_three_seasons$runs)
fit_and_plot_normal(runs_data, 'Normal Distribution of Runs Scored by Top Batsmen (Last 3 IPL Seasons)', 'Runs')


# Remove outliers and fit normal distribution for wickets taken by top bowlers in the last three seasons
wickets_data <- remove_outliers(top_bowlers_last_three_seasons$wickets)
fit_and_plot_normal(wickets_data, 'Normal Distribution of Wickets Taken by Top Bowlers (Last 3 IPL Seasons)', 'Wickets')



# Function to apply Box-Cox transformation
boxcox_transform <- function(data) {
  data_positive <- data + 1  # Adding 1 to avoid negative values
  # Create a pseudo-response variable and fit a linear model
  bc <- boxcox(data_positive ~ 1, plotit = FALSE)
  lambda <- bc$x[which.max(bc$y)]
  transformed_data <- ((data_positive^lambda - 1) / lambda)
  return(transformed_data)
}

# Apply Box-Cox transformation and fit normal distribution for runs scored by top batsmen in the last three seasons
runs_data_bc <- boxcox_transform(runs_data)
fit_and_plot_normal(runs_data_bc, 'Box-Cox Normal Distribution of Runs Scored by Top Batsmen (Last 3 IPL Seasons)', 'Box-Cox(Runs)')

# Apply Box-Cox transformation and fit normal distribution for wickets taken by top bowlers in the last three seasons
wickets_data_bc <- boxcox_transform(wickets_data)
fit_and_plot_normal(wickets_data_bc, 'Box-Cox Normal Distribution of Wickets Taken by Top Bowlers (Last 3 IPL Seasons)', 'Box-Cox(Wickets)')


# Function to perform and print results of normality tests
normality_tests <- function(data, data_label) {
  cat(paste("\nNormality tests for", data_label, ":\n"))

  # Shapiro-Wilk Test
  shapiro_test <- shapiro.test(data)
  cat(paste("Shapiro-Wilk Test: Statistics =", round(shapiro_test$statistic, 3), ", p-value =", round(shapiro_test$p.value, 3), "\n"))

  # Kolmogorov-Smirnov Test
  ks_test <- ks.test(data, "pnorm", mean(data), sd(data))
  cat(paste("Kolmogorov-Smirnov Test: Statistics =", round(ks_test$statistic, 3), ", p-value =", round(ks_test$p.value, 3), "\n"))

  # Anderson-Darling Test
  ad_test <- ad.test(data)
  cat(paste("Anderson-Darling Test: Statistics =", round(ad_test$statistic, 3), "\n"))

  # Extract and print critical values
  ad_critical_values <- c(0.576, 0.656, 0.787, 0.918, 1.092)  # Standard critical values for Anderson-Darling test
  ad_levels <- c(15, 10, 5, 2.5, 1)  # Corresponding significance levels  

  for (i in seq_along(ad_levels)) {
    sig_level <- ad_levels[i]
    crit_val <- ad_critical_values[i]
    cat(paste("  At", sig_level, "% significance level: critical value =", round(crit_val, 3), "\n"))
  }
}

# Perform normality tests for runs scored by top batsmen in the last three seasons
normality_tests(runs_data_bc, 'Box-Cox Transformed Runs Scored by Top Batsmen')

# Perform normality tests for wickets taken by top bowlers in the last three seasons
normality_tests(wickets_data_bc, 'Box-Cox Transformed Wickets Taken by Top Bowlers')


library(readxl)
# Load the salary data
setwd('C:\\Users\\SPURGE\\Desktop\\SCMA')

salary_data<-read_excel('IPL SALARIES 2024.xlsx')


# Display the first few rows of the salary data to understand its structure
head(salary_data)

# Convert the salaries from lakh rupees to actual rupees
salary_data$Salary <- salary_data$Rs * 100000  # 1 lakh = 100000


# Filter the IPL data for "P Simran singh"
P_Simran_singh_data <- ipl_data %>%
  filter(Striker == "P Simran singh" | Bowler == "P Simran singh")

# Replace "P Simran singh" with "Prabhsimran Singh" in the filtered data
P_Simran_singh_data$Striker <- gsub("P Simran singh", "Prabhsimran Singh", P_Simran_singh_data$Striker)
P_Simran_singh_data$Bowler <- gsub("P Simran singh", "Prabhsimran Singh", P_Simran_singh_data$Bowler)


# Merge the filtered and renamed IPL data with the salary data
merged_data <- left_join(P_Simran_singh_data, salary_data, by = c("Striker" = "Player"))

# Calculate total runs scored by "Prabhsimran Singh" in 2023
total_runs_2023 <- sum(merged_data %>% filter(Season == "2023" & Striker == "Prabhsimran Singh") %>% pull(runs_scored), na.rm = TRUE)

# Calculate total wickets taken by "Prabhsimran Singh" in 2023
total_wickets_2023 <- sum(merged_data %>% filter(Season == "2023" & Bowler == "Prabhsimran Singh") %>% pull(wicket_confirmation), na.rm = TRUE)


# Add a new column for performance in 2023 in the salary data
salary_data <- salary_data %>%
  mutate(Performance_2023_Runs = ifelse(Player == "Prabhsimran Singh", total_runs_2023, NA),
         Performance_2023_Wickets = ifelse(Player == "Prabhsimran Singh", total_wickets_2023, NA))


# Now, filter data for 2024
P_Simran_singh_2024_data <- merged_data %>%
  filter(Season == "2024")

# Add the salary based on performance in 2023
P_Simran_singh_2024_data$Salary_2024 <- salary_data %>% filter(Player == "Prabhsimran Singh") %>% pull(Salary)


# Calculate total runs and wickets in 2024
total_runs_2024 <- sum(P_Simran_singh_2024_data %>% filter(Striker == "Prabhsimran Singh") %>% pull(runs_scored), na.rm = TRUE)
total_wickets_2024 <- sum(P_Simran_singh_2024_data %>% filter(Bowler == "Prabhsimran Singh") %>% pull(wicket_confirmation), na.rm = TRUE)


# Print total runs and wickets for 2024 and the salary for 2024
cat(paste("Total Runs Scored by Prabhsimran Singh in 2024:", total_runs_2024, "\n"))
cat(paste("Total Wickets Taken by Prabhsimran Singh in 2024:", total_wickets_2024, "\n"))
cat(paste("Salary Paid to Prabhsimran Singh in 2024:", P_Simran_singh_2024_data$Salary_2024[1], "\n"))


# Analysis: Relationship between salary based on 2023 performance and 2024 performance
cat(paste("Performance in 2023 (Total Runs):", total_runs_2023, "\n"))
cat(paste("Performance in 2023 (Total Wickets):", total_wickets_2023, "\n"))
cat(paste("Performance in 2024 (Total Runs):", total_runs_2024, "\n"))
cat(paste("Performance in 2024 (Total Wickets):", total_wickets_2024, "\n"))

# Create a dataframe to visualize
performance_data <- data.frame(
  Year = c("2023", "2024"),
  Total_Runs = c(total_runs_2023, total_runs_2024),
  Total_Wickets = c(total_wickets_2023, total_wickets_2024),
  Salary = c(NA, P_Simran_singh_2024_data$Salary_2024[1])
)

print(performance_data)

# Plotting the relationship
dev.off() # Reset the plotting device
par(mfrow = c(2, 1)) # Set the layout to 2 rows, 1 column
barplot(performance_data$Total_Runs, names.arg = performance_data$Year, col = "blue", main = "Performance of Prabhsimran Singh(Total Runs in 2023 and 2024)", xlab = "Year", ylab = "Total Runs Scored")
barplot(performance_data$Total_Wickets, names.arg = performance_data$Year, col = "green", main = "Performance of Prabhsimran Singh(Total Wickets in 2023 and 2024)", xlab = "Year", ylab = "Total Wickets Taken")



