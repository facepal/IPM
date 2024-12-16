# Clear environment (if needed)
rm(list = ls())

# Load required packages
library(tidyr)
library(dplyr)
library(readxl)
library(lubridate)
library(stringr)

# --------------------------
# 1. Data Import
# --------------------------
# Import mutual fund return data
data <- read_xlsx("D:\\Studia\\5 rok\\IPM\\Project_IPM\\Data_IPM.xlsx", 
                  sheet = "Return_Index" ,
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>% 
  filter(Date >= as.Date("2008-12-01") & Date <= as.Date("2022-12-31")) 


# Calculate monthly returns from total return indices
monthly_data <- data %>%
  group_by(month = floor_date(Date, "month")) %>%
  filter(Date == max(Date, na.rm = TRUE)) %>%
  ungroup()

monthly_returns <- monthly_data %>%
  mutate(across(ends_with("TOT RETURN IND"), 
                ~ (. - lag(.)) / lag(.), 
                .names = "rm_{.col}")) %>%
  rename_with(~ str_remove(.x, " - TOT RETURN IND"), starts_with("rm_")) %>%
  select(Date, starts_with("rm_"))

# Import total assets data
assets_data <- read_xlsx("D:\\Studia\\5 rok\\IPM\\Project_IPM\\Data_IPM.xlsx",
                         sheet = "Total_Assets",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>% 
  filter(Date >= as.Date("2009-01-01") & Date <= as.Date("2022-12-31")) 

# Align assets data to monthly
assets_monthly <- assets_data %>%
  group_by(month = floor_date(Date, "month")) %>%
  filter(Date == max(Date, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-Date) %>%
  rename(Date = month)

# Import Fama/French 5-Factor data
ff_factors <- read.csv("D:\\Studia\\5 rok\\IPM\\Project_IPM\\F-F_Research_Data_5_Factors_2x3.csv")
ff_factors <- ff_factors %>%
  mutate(Date = as.Date(paste0(substr(Date, 1, 4), "-", substr(Date, 5, 6), "-01")),
         Mkt.RF = Mkt.RF/100,
         SMB = SMB/100,
         HML = HML/100,
         RMW = RMW/100,
         CMA = CMA/100,
         RF = RF/100) %>%
  filter(Date >= as.Date("2009-01-01") & Date <= as.Date("2022-12-31"))

# Import Q5 factors
q5_factors <- read.csv("D:\\Studia\\5 rok\\IPM\\Project_IPM\\q5_factors_monthly_2023.csv") 
q5_factors <- q5_factors %>%
  mutate(Date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01"), 
                        format = "%Y-%m-%d"),
         R_F = R_F / 100,
         R_MKT = R_MKT / 100,
         R_ME = R_ME / 100,
         R_IA = R_IA / 100,
         R_ROE = R_ROE / 100,
         R_EG = R_EG / 100  )%>%
  filter(year >= 2009 & year <  2023)

# Align dates in monthly_returns
monthly_returns <- monthly_returns %>%
  mutate(Date = floor_date(Date, "month"))

# Merge all data
factor_data <- ff_factors %>%
  left_join(q5_factors, by = "Date") %>%
  left_join(monthly_returns, by = "Date") %>%
  left_join(assets_monthly, by = "Date")

factor_data <- factor_data %>%
  mutate(across(where(is.character), as.numeric))


# Identify funds
fund_names <- grep("^rm_", names(factor_data), value = TRUE)

# --------------------------
# 2. Rolling Window Estimation
# --------------------------
# Define rolling window length (5 years = 60 months)
window_length <- 60

# Extract available dates
all_dates <- sort(unique(factor_data$Date))
# We will start from a date at least 5 years after the start (e.g., earliest start date)
# The last window ends at the end of data. After each window, we move 12 months forward.
# Example: If start is 2009-01-01, first window: 2009-01 to 2013-12, 
# then next window: 2010-01 to 2014-12, etc.
# Ensure we have enough data for rolling windows.
start_index <- which(all_dates == as.Date("2009-01-01"))
end_index <- length(all_dates)

# We'll iterate by year steps. For each iteration:
#  - window_end is the end of the 5-year window
#  - window_start is 60 months prior
# After selecting top/bottom funds, we’ll store them.

# Define significance level
alpha_level <- 0.05

# Variables to count how many alpha estimates are significant and total tested
significant_alpha_count <- 0
total_alpha_count <- 0


results_list <- list()

for(i in seq(start_index + window_length - 1, end_index, by = 12)){
  # i is the index of the end of the window
  if(i > end_index) break
  
  window_end_date <- all_dates[i]
  window_start_date <- all_dates[i - (window_length - 1)]
  
  # Subset data for this window
  window_data <- factor_data %>%
    filter(Date >= window_start_date & Date <= window_end_date)
  
  # Check that we have a full 60-month window
  if(nrow(window_data) < window_length) next
  
  # Estimate alphas for each fund 
  # You can choose another model if needed. ctrl+sfit+c for multiple row comment 
  
  # Fama/French 5-Factor model 
  # alphas <- sapply(fund_names, function(fund){
  #   model <- lm(window_data[[fund]] ~ Mkt.RF + SMB + HML + RMW + CMA,
  #               data = window_data)
  #   coef(model)[1]
  # })
  
  # Single Index Model 
  # alphas <- sapply(fund_names, function(fund){
  #   model <- lm(window_data[[fund]] ~ Mkt.RF,
  #               data = window_data)
  #   coef(model)[1]
  # })
  
  # Q5 Model
  alphas <- sapply(fund_names, function(fund){
    model <- lm(window_data[[fund]] ~ R_MKT + R_ME + R_IA + R_ROE + R_EG,
                data = window_data)
    coef(model)[1]
  })

  names(alphas) <- fund_names
  
  # Rank funds by alpha
  alpha_ranks <- sort(alphas, decreasing = TRUE)
  
  # Select top 5 and bottom 5
  top5 <- names(alpha_ranks)[1:5]
  bottom5 <- names(alpha_ranks)[(length(alpha_ranks)-4):length(alpha_ranks)]
  
  # Store results
  results_list[[as.character(window_end_date)]] <- list(
    window_start = window_start_date,
    window_end = window_end_date,
    top5 = top5,
    bottom5 = bottom5
  )
  # for (fund in fund_names) {
  # model <- lm(window_data[[fund]] ~ window_data$Mkt.RF + window_data$SMB + 
  #               window_data$HML + window_data$RMW + window_data$CMA)
  # # Extract the summary
  # model_summary <- summary(model)
  # # Intercept row: "(Intercept)"
  # intercept_p_value <- model_summary$coefficients["(Intercept)", "Pr(>|t|)"]
  # 
  # # Update counts
  # total_alpha_count <- total_alpha_count + 1
  # if (!is.na(intercept_p_value) && intercept_p_value < alpha_level) {
  #   significant_alpha_count <- significant_alpha_count + 1
  # }
  selected_funds <- c(top5, bottom5)
  for (fund in selected_funds) {
    # Fit the model again to extract p-values
    model <- lm(window_data[[fund]] ~ R_MKT + R_ME + R_IA + R_ROE + R_EG ,
                data = window_data)
    model_summary <- summary(model)
    intercept_p_value <- model_summary$coefficients["(Intercept)", "Pr(>|t|)"]

    # Update counts
    total_alpha_count <- total_alpha_count + 1
    if (!is.na(intercept_p_value) && intercept_p_value < alpha_level) {
      significant_alpha_count <- significant_alpha_count + 1
    }
  }
  
  }
  

# After the loop ends, print results
cat("Total number of alpha estimates tested (Top5 & Bottom5 only):", total_alpha_count, "\n")
cat("Number of significant alphas (p <", alpha_level, "):", significant_alpha_count, "\n")
cat("Percentage of significant alphas:", (significant_alpha_count / total_alpha_count) * 100, "%\n")
