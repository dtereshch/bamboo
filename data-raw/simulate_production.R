# Set seed for reproducibility
set.seed(123)

# Parameters
n_firms <- 30
n_years <- 6 # still 6 periods internally
total_obs <- n_firms * n_years

# Generate firm and year identifiers (years 1 to 6 internally)
firm <- rep(1:n_firms, each = n_years)
year <- rep(1:n_years, times = n_firms)

# Create base data frame
panel_data <- data.frame(firm = firm, year = year)

# Split firms into two groups: complete and with entry/exit
n_complete <- floor(n_firms * 0.5) # 50% of firms have complete data
complete_firms <- sample(1:n_firms, n_complete)
entry_exit_firms <- setdiff(1:n_firms, complete_firms)

# Initialize active status
panel_data$active <- TRUE

# For entry/exit firms, simulate different start and end years
if (length(entry_exit_firms) > 0) {
  # Start years: more firms start in early years
  start_years <- sample(
    1:3,
    length(entry_exit_firms),
    replace = TRUE,
    prob = c(0.6, 0.3, 0.1)
  )

  # Duration: firms stay for different periods (minimum 2 years)
  durations <- sample(
    2:6,
    length(entry_exit_firms),
    replace = TRUE,
    prob = c(0.1, 0.2, 0.3, 0.3, 0.1)
  )

  # Calculate end years
  end_years <- pmin(n_years, start_years + durations - 1)

  # Mark observations outside firm's active period as inactive
  for (i in seq_along(entry_exit_firms)) {
    firm_id <- entry_exit_firms[i]
    start_yr <- start_years[i]
    end_yr <- end_years[i]

    panel_data$active[
      panel_data$firm == firm_id &
        (panel_data$year < start_yr | panel_data$year > end_yr)
    ] <- FALSE
  }
}

# Assign initial industries to firms
industries <- c("Industry 1", "Industry 2", "Industry 3")
industry_probs <- c(0.4, 0.35, 0.25)
initial_industries <- sample(
  industries,
  n_firms,
  replace = TRUE,
  prob = industry_probs
)

# Create industry column with initial assignments
panel_data$industry <- factor(
  rep(initial_industries, each = n_years),
  levels = industries
)

# Simulate occasional industry changes over time
firms_that_change <- sample(1:n_firms, size = round(n_firms * 0.2))
change_probability <- 0.15

for (firm_id in firms_that_change) {
  firm_indices <- which(panel_data$firm == firm_id & panel_data$active)

  for (idx in firm_indices) {
    if (panel_data$year[idx] > 1) {
      if (runif(1) < change_probability) {
        current_industry <- as.character(panel_data$industry[idx])
        possible_new_industries <- setdiff(industries, current_industry)
        new_industry <- sample(possible_new_industries, 1)
        future_indices <- firm_indices[firm_indices >= idx]
        panel_data$industry[future_indices] <- new_industry
        break
      }
    }
  }
}

# True Cobb-Douglas parameters (by industry)
alpha_values <- c(0.25, 0.35, 0.3)
beta_values <- c(0.65, 0.55, 0.6)
A_values <- c(2.0, 2.2, 1.8)

# Generate firm-specific effects
firm_effects <- rnorm(n_firms, mean = 0, sd = 0.5)

# Generate capital with industry-specific trends
capital_base <- exp(rnorm(total_obs, mean = 3 + firm_effects[firm], sd = 0.8))
capital_growth <- numeric(total_obs)
for (i in 1:total_obs) {
  industry_idx <- which(industries == panel_data$industry[i])
  capital_growth[i] <- 1 +
    (0.03 + 0.02 * industry_idx) * (panel_data$year[i] - 1)
}
panel_data$capital <- capital_base * capital_growth

# Generate labor with industry-specific trends
labor_base <- exp(rnorm(total_obs, mean = 4 + firm_effects[firm], sd = 0.7))
labor_growth <- numeric(total_obs)
for (i in 1:total_obs) {
  industry_idx <- which(industries == panel_data$industry[i])
  labor_growth[i] <- 1 + (0.02 + 0.01 * industry_idx) * (panel_data$year[i] - 1)
}
panel_data$labor <- labor_base * labor_growth

# Generate sales using Cobb-Douglas function
technology_shock <- rnorm(total_obs, mean = 0, sd = 0.1)
panel_data$sales <- numeric(total_obs)

for (i in 1:total_obs) {
  industry_idx <- which(industries == panel_data$industry[i])
  alpha <- alpha_values[industry_idx]
  beta <- beta_values[industry_idx]
  A <- A_values[industry_idx]
  panel_data$sales[i] <- A *
    (panel_data$capital[i]^alpha) *
    (panel_data$labor[i]^beta) *
    exp(technology_shock[i])
}

# Set NAs for firms outside their active period
panel_data$sales[!panel_data$active] <- NA
panel_data$capital[!panel_data$active] <- NA
panel_data$labor[!panel_data$active] <- NA
panel_data$industry[!panel_data$active] <- NA

# Add random missing values (approx. 2% of remaining non-NA values)
non_na_indices <- which(panel_data$active)

sales_na_indices <- sample(
  non_na_indices,
  size = round(0.02 * length(non_na_indices))
)
panel_data$sales[sales_na_indices] <- NA

capital_na_indices <- sample(
  non_na_indices,
  size = round(0.02 * length(non_na_indices))
)
panel_data$capital[capital_na_indices] <- NA

labor_na_indices <- sample(
  non_na_indices,
  size = round(0.02 * length(non_na_indices))
)
panel_data$labor[labor_na_indices] <- NA

# ------------------------------------------------------------
# Generate ownership variable (stable but with occasional changes)
# ------------------------------------------------------------
ownership_levels <- c("private", "public", "mixed")
# Initial ownership probabilities (can be adjusted)
ownership_probs <- c(0.5, 0.3, 0.2)

# Initialize ownership vector
panel_data$ownership <- factor(NA, levels = ownership_levels)

# For each firm, generate ownership over time
for (f in 1:n_firms) {
  firm_rows <- which(panel_data$firm == f)

  # Year 1 ownership
  current_own <- sample(ownership_levels, 1, prob = ownership_probs)
  panel_data$ownership[firm_rows[1]] <- current_own

  # Subsequent years: small probability of change
  for (t in 2:n_years) {
    if (runif(1) < 0.05) {
      # 5% chance of change per year
      # Change to a different ownership category
      other_levels <- setdiff(ownership_levels, current_own)
      current_own <- sample(other_levels, 1)
    }
    panel_data$ownership[firm_rows[t]] <- current_own
  }
}

# Set ownership to NA for inactive periods
panel_data$ownership[!panel_data$active] <- NA

# ------------------------------------------------------------
# Remap years: internal 1..6 -> observed 1,2,3,5,6,7
# ------------------------------------------------------------
year_mapping <- c(1, 2, 3, 5, 6, 7)
panel_data$year <- year_mapping[panel_data$year]

# ------------------------------------------------------------
# Insert completely missing year 4 for all firms
# ------------------------------------------------------------
year4_rows <- data.frame(
  firm = rep(1:n_firms, each = 1),
  year = 4,
  active = FALSE,
  industry = factor(NA, levels = industries),
  capital = NA_real_,
  labor = NA_real_,
  sales = NA_real_,
  ownership = factor(NA, levels = ownership_levels)
)

# Combine and sort
panel_data <- rbind(panel_data, year4_rows)
panel_data <- panel_data[order(panel_data$firm, panel_data$year), ]

# Remove the 'active' column and reorder
production <- panel_data[, c(
  "firm",
  "year",
  "industry",
  "sales",
  "capital",
  "labor",
  "ownership"
)]

# Save to data/ directory
save(production, file = "data/production.rda", compress = "xz")
