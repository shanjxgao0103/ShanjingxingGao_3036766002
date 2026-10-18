# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plm)
library(haven)
library(kableExtra)
library(webshot2)
library(gridExtra) 

# -----------------------------------------------------------------------------
# 1. Data Loading and Cleaning
# Assuming execution from the /scripts directory
# -----------------------------------------------------------------------------

# Load the dataset and codebook using relative paths
df <- read.table("../data/public.dat")
codebook <- read_lines("../data/codebook")

# Use regex to match the beginning of each line followed by a space
pattern <- "^\\S+(?=  )"

# Find the matches using gregexpr
matches <- gregexpr(pattern, codebook, perl = TRUE)

# Extract the matches
matched_words <- regmatches(codebook, matches)

# Convert list to vector for easier manipulation
matched_words <- unlist(matched_words)  

# Remove blank or whitespace-only elements
new_column_names <- matched_words[matched_words != "" & !grepl("^\\s*\$", matched_words)]

# Assign extracted names to the data frame
colnames(df) <- new_column_names

# Find all character vectors in df and change to numeric 
df <- df |> 
  mutate(across(where(is.character), ~ as.numeric(replace(., . == ".", NA))))

# -----------------------------------------------------------------------------
# 2. Table 2: Means of Key Variables
# -----------------------------------------------------------------------------

# Helper function to calculate row statistics and t-tests dynamically
calc_stat <- function(data, var_name, expr, is_pct = FALSE) {
  
  # Evaluate the target variable
  temp <- data |> 
    mutate(target = {{ expr }}) |> 
    filter(!is.na(target))
  
  # Multiply by 100 if the table reports it as a percentage
  if (is_pct) {
    temp <- temp |> mutate(target = target * 100)
  }
  
  # Extract values based on STATE (Assuming 1 = NJ, 0 = PA)
  nj_vals <- temp |> filter(STATE == 1) |> pull(target)
  pa_vals <- temp |> filter(STATE == 0) |> pull(target)
  
  # Calculate means
  nj_mean <- mean(nj_vals)
  pa_mean <- mean(pa_vals)
  
  # Calculate unpooled t-statistic manually to prevent t.test() errors 
  # when variance is exactly zero (e.g., Wage = \$4.25 in Wave 2 for NJ)
  nj_var <- var(nj_vals)
  pa_var <- var(pa_vals)
  nj_n <- length(nj_vals)
  pa_n <- length(pa_vals)
  
  se_diff <- sqrt((nj_var / nj_n) + (pa_var / pa_n))
  
  t_val <- if(is.na(se_diff) || se_diff == 0) NA else (nj_mean - pa_mean) / se_diff
  
  # Return formatted tibble row
  tibble(
    Variable = var_name,
    NJ = round(nj_mean, 2),
    PA = round(pa_mean, 2),
    t = round(t_val, 2)
  )
}

# Build Section 1: Distribution of Store Types
sec1 <- bind_rows(
  calc_stat(df, "a. Burger King", CHAIN == 1, TRUE),
  calc_stat(df, "b. KFC", CHAIN == 2, TRUE),
  calc_stat(df, "c. Roy Rogers", CHAIN == 3, TRUE),
  calc_stat(df, "d. Wendys", CHAIN == 4, TRUE),
  calc_stat(df, "e. Company-owned", CO_OWNED == 1, TRUE)
)

# Build Section 2: Means in Wave 1
sec2 <- bind_rows(
  calc_stat(df, "a. FTE employment", EMPFT + NMGRS + 0.5 * EMPPT, FALSE),
  calc_stat(df, "b. Percentage full-time employees", (EMPFT + NMGRS) / (EMPFT + EMPPT + NMGRS), TRUE),
  calc_stat(df, "c. Starting wage", WAGE_ST, FALSE),
  calc_stat(df, "d. Wage = \$4.25 (percentage)", WAGE_ST == 4.25, TRUE),
  calc_stat(df, "e. Price of full meal", PENTREE + PFRY + PSODA, FALSE),
  calc_stat(df, "f. Hours open (weekday)", HRSOPEN, FALSE)
)

# Build Section 3: Means in Wave 2
sec3 <- bind_rows(
  calc_stat(df, "a. FTE employment", EMPFT2 + NMGRS2 + 0.5 * EMPPT2, FALSE),
  calc_stat(df, "b. Percentage full-time employees", (EMPFT2 + NMGRS2) / (EMPFT2 + EMPPT2 + NMGRS2), TRUE),
  calc_stat(df, "c. Starting wage", WAGE_ST2, FALSE),
  calc_stat(df, "d. Wage = \$4.25 (percentage)", WAGE_ST2 == 4.25, TRUE),
  calc_stat(df, "e. Wage = \$5.05 (percentage)", WAGE_ST2 == 5.05, TRUE),
  calc_stat(df, "f. Price of full meal", PENTREE2 + PFRY2 + PSODA2, FALSE),
  calc_stat(df, "g. Hours open (weekday)", HRSOPEN2, FALSE)
)

# Combine and Format the Output Table
table2 <- bind_rows(sec1, sec2, sec3)

table2_formatted <- table2 |>
  kable(caption = "Table 2 - Means of Key Variables", row.names = FALSE) |>
  kable_classic(full_width = FALSE, html_font = "Cambria") |>
  pack_rows("1. Distribution of Store Types (percentages)", 1, 5) |>
  pack_rows("2. Means in Wave 1", 6, 11) |>
  pack_rows("3. Means in Wave 2", 12, 18)

# Save the table to the output folder
table2_formatted |>
  save_kable(file = "../output/table2_replication.png", zoom = 1.5)

# -----------------------------------------------------------------------------
# 3. Table 3: Average Employment Per Store Before and After the Rise
# -----------------------------------------------------------------------------

# Data Preparation: Properly target all temporary closure codes
df_clean <- df |>
  mutate(
    FTE1 = EMPFT + NMGRS + 0.5 * EMPPT,
    FTE2 = EMPFT2 + NMGRS2 + 0.5 * EMPPT2,
    
    # 0 = Refusal. 3 = Permanently closed (already 0 in data).
    # 2, 4, 5 = The 4 specific temporarily closed / renovation stores.
    FTE2_r5 = case_when(
      is.na(FTE2) & STATUS2 %in% c(2, 4, 5) ~ 0,
      TRUE ~ FTE2
    )
  )

# Helper function to calculate means and corrected standard errors
calc_table3_col <- function(data) {
  
  # Local helper for standard error of a single mean
  calc_se <- function(x) {
    x_clean <- x[!is.na(x)]
    if(length(x_clean) == 0) return(NA)
    sd(x_clean) / sqrt(length(x_clean))
  }
  
  # Row 1: FTE employment before
  m1 <- mean(data[["FTE1"]], na.rm = TRUE)
  se1 <- calc_se(data[["FTE1"]])
  
  # Row 2: FTE employment after
  m2 <- mean(data[["FTE2"]], na.rm = TRUE)
  se2 <- calc_se(data[["FTE2"]])
  
  # Row 3: Change in mean FTE (accounting for covariance in the overlapping sample)
  m3 <- m2 - m1
  
  var1 <- var(data[["FTE1"]], na.rm = TRUE)
  var2 <- var(data[["FTE2"]], na.rm = TRUE)
  n1 <- sum(!is.na(data[["FTE1"]]))
  n2 <- sum(!is.na(data[["FTE2"]]))
  
  bal_data <- data |> filter(!is.na(FTE1) & !is.na(FTE2))
  cov12 <- cov(bal_data[["FTE1"]], bal_data[["FTE2"]])
  n12 <- nrow(bal_data)
  
  # Variance formula for difference of means with an overlapping sample
  var_diff <- (var1 / n1) + (var2 / n2) - 2 * (n12 / (n1 * n2)) * cov12
  se3 <- sqrt(var_diff)
  
  # Row 4: Balanced sample (Paired difference)
  diff4 <- bal_data[["FTE2"]] - bal_data[["FTE1"]]
  m4 <- mean(diff4)
  se4 <- calc_se(diff4)
  
  # Row 5: Temporarily closed set to 0 (Paired difference on expanded sample)
  bal5 <- data |> filter(!is.na(FTE1) & !is.na(FTE2_r5))
  diff5 <- bal5[["FTE2_r5"]] - bal5[["FTE1"]]
  m5 <- mean(diff5)
  se5 <- calc_se(diff5)
  
  tibble(
    mean = c(m1, m2, m3, m4, m5),
    se = c(se1, se2, se3, se4, se5)
  )
}

# Create subsets corresponding to the Table 3 columns
pa_data       <- df_clean |> filter(STATE == 0)
nj_data       <- df_clean |> filter(STATE == 1)
nj_low_data   <- df_clean |> filter(STATE == 1 & WAGE_ST == 4.25)
nj_mid_data   <- df_clean |> filter(STATE == 1 & WAGE_ST >= 4.26 & WAGE_ST <= 4.99)
nj_high_data  <- df_clean |> filter(STATE == 1 & WAGE_ST >= 5.00)

# Calculate statistics for each subset
stats_pa   <- calc_table3_col(pa_data)
stats_nj   <- calc_table3_col(nj_data)
stats_low  <- calc_table3_col(nj_low_data)
stats_mid  <- calc_table3_col(nj_mid_data)
stats_high <- calc_table3_col(nj_high_data)

# Helper function for calculating unpooled Difference columns
calc_diff <- function(df1, df2) {
  tibble(
    mean = df1[["mean"]] - df2[["mean"]],
    se = sqrt(df1[["se"]]^2 + df2[["se"]]^2)
  )
}

# Calculate the difference columns (NJ - PA)
stats_diff_nj_pa    <- calc_diff(stats_nj, stats_pa)
stats_diff_low_high <- calc_diff(stats_low, stats_high)
stats_diff_mid_high <- calc_diff(stats_mid, stats_high)

# Helper function to format final string output as "Mean (SE)"
format_col <- function(df) {
  sprintf("%.2f (%.2f)", df[["mean"]], df[["se"]])
}

# Assemble the final tibble matching the paper's exact column layout
table3 <- tibble(
  Variable = c(
    "1. FTE employment before, all available observations",
    "2. FTE employment after, all available observations",
    "3. Change in mean FTE employment",
    "4. Change in mean FTE employment, balanced sample of stores",
    "5. Change in mean FTE employment, setting FTE at temporarily closed stores to 0"
  ),
  `PA (i)` = format_col(stats_pa),
  `NJ (ii)` = format_col(stats_nj),
  `Difference, NJ-PA (iii)` = format_col(stats_diff_nj_pa),
  `Wage = $4.25 (iv)` = format_col(stats_low),
  `Wage = $4.26-$4.99 (v)` = format_col(stats_mid),
  `Wage >= $5.00 (vi)` = format_col(stats_high),
  `Low-high (vii)` = format_col(stats_diff_low_high),
  `Midrange-high (viii)` = format_col(stats_diff_mid_high)
)

# Output with proper headers and formatting using Kable
table3_formatted <- table3 |>
  kable(
    caption = "Table 3 - Average Employment Per Store Before and After the Rise in New Jersey Minimum Wage",
    row.names = FALSE,
    align = c("l", rep("c", 8))
  ) |>
  kable_classic(full_width = FALSE, html_font = "Cambria") |>
  add_header_above(c(
    " " = 1,
    "Stores by state" = 2,
    " " = 1,
    "Stores in New Jersey" = 3,
    "Differences within NJ" = 2
  )) |>
  column_spec(1, width = "350px")

# Save the table to the output folder
table3_formatted |>
  save_kable(file = "../output/table3_replication.png", zoom = 1.5)

# -----------------------------------------------------------------------------
# 4. Table 4: Reduced Form Models for Change in Employment
# -----------------------------------------------------------------------------

# Data Preparation: The Exact N=357 Filter
final_df <- df |>
  mutate(
    FTE_EMP1 = EMPFT + NMGRS + 0.5 * EMPPT,
    FTE_EMP2 = EMPFT2 + NMGRS2 + 0.5 * EMPPT2,
    delta_emp = FTE_EMP2 - FTE_EMP1, 
    
    # GAP is 0 for PA, 0 for NJ >= 5.05, and proportional increase for NJ < 5.05
    GAP = ifelse(STATE == 1 & WAGE_ST < 5.05, (5.05 - WAGE_ST) / WAGE_ST, 0),
    
    CHAIN1 = ifelse(CHAIN == 1, 1, 0),
    CHAIN2 = ifelse(CHAIN == 2, 1, 0),
    CHAIN3 = ifelse(CHAIN == 3, 1, 0)
  ) |>
  # CRITICAL FIX: Explicitly enforce the paper's N=357 sample restriction.
  # Both employment AND wages must be available in BOTH waves.
  filter(!is.na(FTE_EMP1) & !is.na(FTE_EMP2) & !is.na(WAGE_ST) & !is.na(WAGE_ST2))

# Regressions
reg1 <- lm(delta_emp ~ STATE, data = final_df)
reg2 <- lm(delta_emp ~ STATE + CHAIN1 + CHAIN2 + CHAIN3 + CO_OWNED, data = final_df)
reg3 <- lm(delta_emp ~ GAP, data = final_df)
reg4 <- lm(delta_emp ~ GAP + CHAIN1 + CHAIN2 + CHAIN3 + CO_OWNED, data = final_df)

# Model (v) includes GAP, chain/ownership controls, and 4 region dummies.
# By including 2 PA regions (PA1, PA2) and 2 NJ regions (SOUTHJ, CENTRALJ), 
# the 5th region (NORTHJ) serves as the omitted reference group to prevent multicollinearity.
reg5 <- lm(delta_emp ~ GAP + CHAIN1 + CHAIN2 + CHAIN3 + CO_OWNED + SOUTHJ + CENTRALJ + PA1 + PA2, data = final_df)

# Helper function to extract and format table statistics
get_model_stats <- function(model, target_var, has_chain, has_region, base_model = NULL) {
  sm <- summary(model)
  
  # Extract Coefficient and Standard Error cleanly
  if (target_var %in% rownames(sm[["coefficients"]])) {
    coef <- sprintf("%.2f", sm[["coefficients"]][target_var, "Estimate"])
    se <- sprintf("(%.2f)", sm[["coefficients"]][target_var, "Std. Error"])
  } else {
    coef <- "—"
    se <- ""
  }
  
  # Residual standard error
  ser <- sprintf("%.2f", sm[["sigma"]])
  
  # Joint F-test p-value for added controls (compared to the baseline model)
  if (!is.null(base_model)) {
    ftest <- anova(base_model, model)
    pval <- sprintf("%.2f", ftest[["Pr(>F)"]][2])
  } else {
    pval <- "—"
  }
  
  return(c(coef, se, ifelse(has_chain, "yes", "no"), ifelse(has_region, "yes", "no"), ser, pval))
}

# Extract statistics for all columns
col_i   <- get_model_stats(reg1, "STATE", FALSE, FALSE)
col_ii  <- get_model_stats(reg2, "STATE", TRUE,  FALSE, reg1)
col_iii <- get_model_stats(reg3, "GAP",   FALSE, FALSE)
col_iv  <- get_model_stats(reg4, "GAP",   TRUE,  FALSE, reg3)
col_v   <- get_model_stats(reg5, "GAP",   TRUE,  TRUE,  reg4)

# Assemble the Data Frame to match the original layout
table4 <- tibble(
  `Independent variable` = c(
    "1. New Jersey dummy", 
    " ", 
    "2. Initial wage gap", 
    "  ", 
    "3. Controls for chain and ownership", 
    "4. Controls for region", 
    "5. Standard error of regression", 
    "6. Probability value for controls"
  ),
  `(i)`   = c(col_i[1],   col_i[2],   "—",         "",         col_i[3:6]),
  `(ii)`  = c(col_ii[1],  col_ii[2],  "—",         "",         col_ii[3:6]),
  `(iii)` = c("—",        "",         col_iii[1],  col_iii[2], col_iii[3:6]),
  `(iv)`  = c("—",        "",         col_iv[1],   col_iv[2],  col_iv[3:6]),
  `(v)`   = c("—",        "",         col_v[1],    col_v[2],   col_v[3:6])
)

# Output Table using Kable
table4_formatted <- table4 |>
  kable(
    caption = "Table 4 - Reduced-Form Models for Change in Employment",
    align = c("l", rep("c", 5))
  ) |>
  kable_classic(full_width = FALSE, html_font = "Cambria") |>
  add_header_above(c(" " = 1, "Model" = 5))

table4_formatted |>
  save_kable(file = "../output/table4_replication.png", zoom = 1.5)

# -----------------------------------------------------------------------------
# 5. Checking for data validity for table 4
# -----------------------------------------------------------------------------

# Create base variables without dropping rows
diagnostic_df <- df |>
  mutate(
    FTE1 = EMPFT + NMGRS + 0.5 * EMPPT,
    FTE2 = EMPFT2 + NMGRS2 + 0.5 * EMPPT2,
    delta_emp = FTE2 - FTE1
  )

# Count missing values across required columns
missing_analysis <- diagnostic_df |>
  summarise(
    Total_Stores = n(),
    Missing_FTE1 = sum(is.na(FTE1)),
    Missing_FTE2 = sum(is.na(FTE2)),
    Missing_WAGE1 = sum(is.na(WAGE_ST)),
    Missing_WAGE2 = sum(is.na(WAGE_ST2)),
    Valid_Sample_Size = sum(!is.na(FTE1) & !is.na(FTE2) & !is.na(WAGE_ST) & !is.na(WAGE_ST2))
  )

print("--- Missing Data Breakdown ---")
print(missing_analysis)

# Filter data using strict inclusion criteria
final_table4_df <- diagnostic_df |>
  filter(!is.na(FTE1) & !is.na(FTE2) & !is.na(WAGE_ST) & !is.na(WAGE_ST2))

# Compare dependent variable against the paper's exact targets
target_check <- final_table4_df |>
  summarise(
    Actual_N = n(),
    Target_N = 357,
    Actual_Mean = round(mean(delta_emp), 3),
    Target_Mean = -0.237,
    Actual_SD = round(sd(delta_emp), 3),
    Target_SD = 8.825
  )

# Output and save diagnostic table
target_check |>
  kable(caption = "Target Verification for Table 4", align = "c") |>
  kable_classic(full_width = FALSE, html_font = "Cambria") |>
  save_kable(file = "../output/table4_diagnostic.png", zoom = 1.5)

# -----------------------------------------------------------------------------
# 6. Figure 1
# -----------------------------------------------------------------------------

# Create Plot 1: February 1992
p1 <- df |>
  filter(!is.na(WAGE_ST)) |>
  mutate(state = factor(STATE, levels = c(1, 0), 
                        labels = c("New Jersey", "Pennsylvania"))) |>
  ggplot(aes(x = WAGE_ST, fill = state)) +
  # width * density * 100 calculates the percentage WITHIN each state group
  geom_histogram(aes(y = after_stat(width * density * 100)),
                 position = position_dodge(preserve = "single"), 
                 binwidth = 0.1, boundary = 4.2) + 
  scale_x_continuous(breaks = seq(4.25, 5.55, by = 0.1)) +
  scale_fill_manual(values = c("New Jersey" = "black", "Pennsylvania" = "gray70")) +
  labs(title = "February 1992", x = "Wage Range", y = "Percent of Stores", fill = "") +
  theme_classic() +
  theme(legend.position = "none") # Hide top legend to match paper

# Create Plot 2: November 1992
p2 <- df |>
  filter(!is.na(WAGE_ST2)) |>
  mutate(state = factor(STATE, levels = c(1, 0), 
                        labels = c("New Jersey", "Pennsylvania"))) |>
  ggplot(aes(x = WAGE_ST2, fill = state)) +
  geom_histogram(aes(y = after_stat(width * density * 100)),
                 position = position_dodge(preserve = "single"), 
                 binwidth = 0.1, boundary = 4.2) + 
  scale_x_continuous(breaks = seq(4.25, 5.55, by = 0.1)) +
  scale_fill_manual(values = c("New Jersey" = "black", "Pennsylvania" = "gray70")) +
  labs(title = "November 1992", x = "Wage Range", y = "Percent of Stores", fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") # Keep legend only on the bottom plot

# Align the two plots vertically using arrangeGrob
figure1 <- arrangeGrob(p1, p2, nrow = 2)

# Save the combined figure to the output folder
ggsave(filename = "../output/Figure1_Replication.png", plot = figure1, 
       width = 8, height = 10, dpi = 300)

# Display in R viewer (optional for execution, but keeps parity with the original script)
grid.arrange(p1, p2, nrow = 2)