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
                  sheet = "Return_Index")
data$Date <- as.Date(data$Date)

# Restrict data to 15 years until December 2023
data <- data %>% filter(Date >= as.Date("2007-12-01") & Date <= as.Date("2023-12-31"))

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
  filter(Date >= as.Date("2007-12-01") & Date <= as.Date("2023-12-31")) 

# Align assets data to monthly (assuming it's also daily and needs similar monthly selection)
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
  filter(Date >= as.Date("2008-01-01") & Date <= as.Date("2023-12-31"))

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
  filter(year >= 2008 & year <  2024)

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

# Identify columns related to Fidelity Magellan
fidelity_cols <- grep("FIDELITY MAGELLAN", names(factor_data), value = TRUE)

# Drop these columns from factor_data
factor_data <- factor_data %>% select(-all_of(fidelity_cols))


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
# Example: If start is 2008-01-01, first window: 2008-01 to 2012-12, 
# then next window: 2009-01 to 2013-12, etc.
# Ensure we have enough data for rolling windows.
start_index <- which(all_dates == as.Date("2008-01-01"))
end_index <- length(all_dates)

# We'll iterate by year steps. For each iteration:
#  - window_end is the end of the 5-year window
#  - window_start is 60 months prior
# After selecting top/bottom funds, we’ll store them.

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
  
  # Estimate alphas for each fund using Fama/French 5-Factor model (example)
  # You can choose another model if needed.
  alphas <- sapply(fund_names, function(fund){
    model <- lm(window_data[[fund]] ~ Mkt.RF + SMB + HML + RMW + CMA, 
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
}

# --------------------------
# 3. Ex-Post Performance of Portfolios
# --------------------------
# For each period, we have top5 and bottom5 funds. Now we need to measure their ex-post returns.
# Assuming "ex-post" means the following year after the window end:
# For example, if window ends at 2012-12, ex-post returns might be from 2013-01 to 2013-12.
# We'll calculate both equally-weighted and value-weighted returns.
# --------------------------
# Revised Ex-Post Performance Computation
# --------------------------
portfolio_returns <- data.frame(
  year = character(),
  portfolio = character(),
  weight_scheme = character(),
  annual_return = numeric(),
  stringsAsFactors = FALSE
)

for(period_end in names(results_list)){
  period_info <- results_list[[period_end]]
  
  # Define ex-post period: next 12 months after period_end window
  ex_post_start <- floor_date(as.Date(period_info$window_end) %m+% months(1), "month")
  ex_post_end <- ex_post_start %m+% months(11)
  
  ex_post_data <- factor_data %>%
    filter(Date >= ex_post_start & Date <= ex_post_end)
  
  # If no data or not enough months, skip
  if(nrow(ex_post_data) == 0) next
  
  # Funds in top5 and bottom5
  top5_funds <- period_info$top5
  bottom5_funds <- period_info$bottom5
  
  # Clean up fund names in alphas if needed (assuming already fixed above)
  # Ensure that top5_funds and bottom5_funds are fund names without "(Intercept)"
  # and correspond to columns in factor_data. If you've implemented the previous fix,
  # this should already be handled.

  
  # Prepare asset column names for value-weighted calculation
  top5_asset_cols <- gsub("^rm_", "", top5_funds)
  top5_asset_cols <- paste0(top5_asset_cols, " - TOTAL NET ASSETS")
  
  bottom5_asset_cols <- gsub("^rm_", "", bottom5_funds)
  bottom5_asset_cols <- paste0(bottom5_asset_cols, " - TOTAL NET ASSETS")
  
  
  # -------------------------------------------------------
  # Equal-Weighted Portfolio Annual Return Calculation
  # -------------------------------------------------------
  # For equal-weighted, each month portfolio return = average of the 5 funds' monthly returns.
  # We do not rebalance monthly because equal weights remain constant (simply 1/5 each fund).
  # Compute monthly portfolio returns
  top5_eq_monthly <- rowMeans(ex_post_data[, top5_funds], na.rm = TRUE)
  bottom5_eq_monthly <- rowMeans(ex_post_data[, bottom5_funds], na.rm = TRUE)
  
  # Annual return = product of (1 + monthly_return) - 1
  top5_eq_annual <- prod(1 + top5_eq_monthly, na.rm = TRUE) - 1
  bottom5_eq_annual <- prod(1 + bottom5_eq_monthly, na.rm = TRUE) - 1
  
  # Store equal-weighted results
  portfolio_returns <- rbind(portfolio_returns, 
                             data.frame(year = ex_post_end,
                                        portfolio = "Top5",
                                        weight_scheme = "Equal",
                                        annual_return = top5_eq_annual))
  
  portfolio_returns <- rbind(portfolio_returns, 
                             data.frame(year = ex_post_end,
                                        portfolio = "Bottom5",
                                        weight_scheme = "Equal",
                                        annual_return = bottom5_eq_annual))
  
  # -------------------------------------------------------
  # Value-Weighted Portfolio Annual Return Calculation
  # -------------------------------------------------------
  # Determine weights at the beginning of the ex-post period (i.e., for the first month)
  # and keep them constant throughout the year.
  
  # Extract asset values for top and bottom funds in the first month of the ex-post period
  first_month_assets_top <- as.numeric(ex_post_data[1, top5_asset_cols])
  first_month_assets_bottom <- as.numeric(ex_post_data[1, bottom5_asset_cols])
  
  total_assets_top <- sum(first_month_assets_top, na.rm = TRUE)
  total_assets_bottom <- sum(first_month_assets_bottom, na.rm = TRUE)
  
  if(total_assets_top > 0){
    weights_top <- first_month_assets_top / total_assets_top
  } else {
    # If no assets, fallback to equal weighting
    weights_top <- rep(1/length(top5_funds), length(top5_funds))
  }
  
  if(total_assets_bottom > 0){
    weights_bottom <- first_month_assets_bottom / total_assets_bottom
  } else {
    weights_bottom <- rep(1/length(bottom5_funds), length(bottom5_funds))
  }
  
  # Compute monthly portfolio returns using these fixed weights
  top5_vw_monthly <- apply(ex_post_data[, top5_funds], 1, function(x) sum(weights_top * x, na.rm = TRUE))
  bottom5_vw_monthly <- apply(ex_post_data[, bottom5_funds], 1, function(x) sum(weights_bottom * x, na.rm = TRUE))
  
  # Annual return = product of (1 + monthly_return) - 1
  top5_vw_annual <- prod(1 + top5_vw_monthly, na.rm = TRUE) - 1
  bottom5_vw_annual <- prod(1 + bottom5_vw_monthly, na.rm = TRUE) - 1
  
  # Store value-weighted results
  portfolio_returns <- rbind(portfolio_returns, 
                             data.frame(year = ex_post_end,
                                        portfolio = "Top5",
                                        weight_scheme = "Value",
                                        annual_return = top5_vw_annual))
  
  portfolio_returns <- rbind(portfolio_returns, 
                             data.frame(year = ex_post_end,
                                        portfolio = "Bottom5",
                                        weight_scheme = "Value",
                                        annual_return = bottom5_vw_annual))
  #browser()
}

# After this loop, `portfolio_returns` will contain the annual returns (over the ex-post year) 
# for top5 and bottom5 portfolios, under both equal and value weighting schemes,
# computed as the product of monthly returns minus one.

#### Presenting the resuts #### 
# Load plotting package
library(dplyr)
library(ggplot2)
library(lubridate)


# 1. Convert `year` column to Date format if necessary:
portfolio_returns <- portfolio_returns %>%
  mutate(year = as.Date(year)) 


# 2. Calculate cumulative returns
portfolio_cumulative <- portfolio_returns %>%
  arrange(year) %>% # sort by year
  group_by(portfolio, weight_scheme) %>%
  mutate(cumulative_return = cumprod(1 + annual_return) - 1) %>%
  ungroup()

# Now `portfolio_cumulative` has a `cumulative_return` column that shows how the portfolio grows over time.

# 3. Plot the results
# A nice plot could have the x-axis as the year, y-axis as the cumulative return,
# color or linetype by portfolio and facet by weight_scheme.

ggplot(portfolio_cumulative, aes(x = year, y = cumulative_return, color = portfolio)) +
  geom_line(size = 1) +
  facet_wrap(~ weight_scheme, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Cumulative Returns of Portfolios Over Time",
       x = "Year",
       y = "Cumulative Return",
       color = "Portfolio") +
  theme_minimal(base_size = 14)

# This plot will give you two panels (one for Equal and one for Value) 
# and lines for Top5 and Bottom5 in each panel.


