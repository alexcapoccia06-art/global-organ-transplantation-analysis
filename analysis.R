# ============================================================
# GLOBAL ORGAN TRANSPLANTATION - STEP 1
# Simplified import for the supplied GODT workbook.
# Original group preparation credited to Alessandro Faglia.
# Portfolio preparation revised for Alessandro Capoccia.
# Original analyses and the labelled personal extension follow.
#
# Run this script from the beginning. Select the ORIGINAL Excel
# workbook when the file-selection window opens.
# If a package is missing, install it once with:
# install.packages(c("readxl", "tidyverse", "janitor"))
# ============================================================

library(readxl)
library(tidyverse)
library(janitor)
library(ggrepel)
# FALSE runs only data preparation. Leave this unchanged for now.
run_analyses <- TRUE

#-----------------------READ THE EXCEL FILE--------------------

# Choose GODT_downloads.xlsx, not the already-tidied workbook.
input_file <- file.choose()

# Later, you can replace file.choose() with a fixed path:
# input_file <- "~/Documents/STATS-1/1187949 2/data/GODT_downloads.xlsx"

# Read the worksheet.
raw_data <- readxl::read_excel(input_file, sheet = "Database")

#-----------------------CLEAN THE COLUMNS----------------------

data <- janitor::clean_names(raw_data)

# Remove spaces at the beginning/end of country and region names.
data$country <- trimws(data$country)
data$region <- trimws(data$region)

# Keep the original population entries for the later checks.
population_reported <- as.character(data$population)

# China and India have entries such as "1,428.20".
# First remove the commas, then convert the result to a number.
data$population <- gsub(",", "", population_reported, fixed = TRUE)
data$population <- as.numeric(data$population)

# Stop if a nonmissing population could not be converted.
if (any(is.na(data$population) & !is.na(population_reported))) {
  stop("Some population entries could not be converted. Check population_reported.")
}

# These populations are already measured in millions.
data <- data %>%
  rename(year = reportyear, population_millions = population)

# The 20 measurement columns that will become indicator/value rows.
indicator_columns <- c(
  "total_actual_dd", "actual_dbd", "actual_dcd",
  "total_utilized_dd", "utilized_dbd", "utilized_dcd",
  "dd_kidney_tx", "ld_kidney_tx", "total_kidney_tx",
  "dd_liver_tx", "domino_liver_tx", "ld_liver_tx", "total_liver_tx",
  "total_heart_tx", "dd_lung_tx", "ld_lung_tx", "total_lung_tx",
  "pancreas_tx", "kidney_pancreas_tx", "small_bowel_tx"
)

# There should be only one source row per country and year.
duplicate_country_year <- data %>%
  count(country, year, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_country_year) > 0) {
  print(duplicate_country_year)
  stop("Duplicate country-year rows: check these before continuing.")
}

#-------------------------TIDY DATA----------------------------

# This full table retains all 20 indicators for each source row,
# including unreported values. It is the canonical exported data.
data_tidy_all <- data %>%
  pivot_longer(cols = all_of(indicator_columns),
               names_to = "indicator", values_to = "value",
               values_drop_na = FALSE) %>%
  mutate(
    category = case_when(
      grepl("actual|utilized", indicator) ~ "donors",
      grepl("_tx$", indicator) ~ "transplants",
      TRUE ~ "other"
    ),
    donor_type = case_when(
      grepl("^dd_", indicator) ~ "deceased_donor",
      grepl("^ld_", indicator) ~ "living_donor",
      grepl("dbd", indicator) ~ "dbd",
      grepl("dcd", indicator) ~ "dcd",
      grepl("domino", indicator) ~ "domino",
      grepl("total", indicator) ~ "total",
      TRUE ~ NA_character_
    ),
    organ = case_when(
      grepl("kidney_pancreas", indicator) ~ "kidney_pancreas",
      grepl("small_bowel", indicator) ~ "small_bowel",
      grepl("kidney", indicator) ~ "kidney",
      grepl("liver", indicator) ~ "liver",
      grepl("heart", indicator) ~ "heart",
      grepl("lung", indicator) ~ "lung",
      grepl("pancreas", indicator) ~ "pancreas",
      TRUE ~ "donor"
    ),
    measure = case_when(
      grepl("actual", indicator) ~ "actual",
      grepl("utilized", indicator) ~ "utilized",
      grepl("_tx$", indicator) ~ "transplants",
      TRUE ~ "other"
    ),
    # Preserve these original aliases for downstream compatibility.
    type = measure,
    donor = case_when(
      grepl("dbd", indicator) ~ "dbd",
      grepl("dcd", indicator) ~ "dcd",
      grepl("^dd_", indicator) ~ "deceased",
      grepl("^ld_", indicator) ~ "living",
      grepl("domino", indicator) ~ "domino",
      TRUE ~ NA_character_
    ),
    # Nonpositive or missing populations cannot support a rate.
    # Keep their reported values; only the derived rate becomes NA.
    value_pmp = value / if_else(
      !is.na(population_millions) & population_millions > 0,
      population_millions, NA_real_
    )
  ) %>%
  arrange(region, country, year, indicator)

invalid_counts <- data_tidy_all %>%
  filter(!is.na(value), value < 0 | value != floor(value)) %>%
  select(country, year, indicator, value)
if (nrow(invalid_counts) > 0) {
  print(head(invalid_counts, 10))
  stop("Negative or noninteger donor/transplant counts. Check the source.",
       call. = FALSE)
}
duplicate_obs <- data_tidy_all %>%
  count(country, year, indicator, name = "n") %>%
  filter(n > 1)
stopifnot(nrow(duplicate_obs) == 0,
          nrow(data_tidy_all) == nrow(data) * length(indicator_columns),
          all(is.na(data_tidy_all$value_pmp) |
                is.finite(data_tidy_all$value_pmp)))

# Existing analyses expect observed values only. Keep their object
# names, while preserving missing observations in data_tidy_all.
data_tidy <- data_tidy_all %>% filter(!is.na(value))
data_tidy2 <- data_tidy
godt_tidy_clean <- data_tidy2
godt_clean <- godt_tidy_clean

#--------------------SOURCE QUALITY DIAGNOSTICS----------------

# Record the population entries whose commas were removed.
population_repairs <- data %>%
  mutate(population_reported = population_reported) %>%
  filter(!is.na(population_reported), grepl(",", population_reported)) %>%
  select(region, country, year, population_reported, population_millions)

# These are review flags, not instructions to replace source counts.
population_issues <- data %>%
  filter(is.na(population_millions) | population_millions <= 0) %>%
  transmute(region, country, year, issue = "invalid_population_for_pmp",
            indicator = "population_millions", value = population_millions,
            comparison_value = NA_real_,
            detail = "Population must be positive to calculate a per-million rate.")

population_repair_log <- population_repairs %>%
  transmute(region, country, year, issue = "population_format_repaired",
            indicator = "population_millions", value = population_millions,
            comparison_value = NA_real_,
            detail = paste("Removed thousands separators from", population_reported))

check_total <- function(total_column, component_columns) {
  complete <- complete.cases(data[, c(total_column, component_columns)])
  component_sum <- rowSums(as.data.frame(data[, component_columns]), na.rm = FALSE)
  data %>%
    mutate(comparison_value = component_sum) %>%
    filter(complete, .data[[total_column]] != comparison_value) %>%
    transmute(region, country, year, issue = "total_component_mismatch",
              indicator = total_column, value = .data[[total_column]],
              comparison_value,
              detail = paste("Compared with", paste(component_columns, collapse = " + ")))
}

component_issues <- bind_rows(
  check_total("total_actual_dd", c("actual_dbd", "actual_dcd")),
  check_total("total_utilized_dd", c("utilized_dbd", "utilized_dcd")),
  check_total("total_kidney_tx", c("dd_kidney_tx", "ld_kidney_tx")),
  check_total("total_liver_tx", c("dd_liver_tx", "ld_liver_tx", "domino_liver_tx")),
  check_total("total_lung_tx", c("dd_lung_tx", "ld_lung_tx"))
)
# In particular, liver discrepancies may reflect different treatment
# of domino transplants. Preserve and review them; do not infer errors.

utilization_issues <- data %>%
  filter(!is.na(total_actual_dd), !is.na(total_utilized_dd),
         total_utilized_dd > total_actual_dd) %>%
  transmute(region, country, year, issue = "utilized_exceeds_actual",
            indicator = "total_utilized_dd", value = total_utilized_dd,
            comparison_value = total_actual_dd,
            detail = "Utilized donors exceed the reported actual donors.")

data_quality_issues <- bind_rows(population_issues, population_repair_log,
                                component_issues, utilization_issues) %>%
  arrange(issue, country, year, indicator)

# The denominator here is countries with a row in this source export
# that year, not an assumed fixed worldwide country count.
reporting_coverage <- data_tidy_all %>%
  group_by(region, year, indicator) %>%
  summarise(
    n_source_countries = n_distinct(country),
    n_reporting = sum(!is.na(value)),
    n_missing = sum(is.na(value)),
    n_zero = sum(value == 0, na.rm = TRUE),
    n_valid_pmp = sum(!is.na(value_pmp)),
    .groups = "drop"
  )

validation_summary <- tibble(
  check = c("source_country_years", "source_countries", "first_year", "last_year",
            "indicators", "full_long_rows", "observed_indicator_rows",
            "missing_indicator_rows", "population_format_repairs_country_years",
            "invalid_population_country_years", "duplicate_observation_keys",
            "total_component_mismatches", "utilized_exceeds_actual"),
  n = c(nrow(data), n_distinct(data$country), min(data$year), max(data$year),
        length(indicator_columns), nrow(data_tidy_all), nrow(godt_clean),
        sum(is.na(data_tidy_all$value)), nrow(population_repairs),
        nrow(population_issues), nrow(duplicate_obs),
        nrow(component_issues), nrow(utilization_issues))
)
print(validation_summary, n = Inf)
#------------------------SAVE STEP 1---------------------------

# Save results in an output folder beside the selected workbook.
output_dir <- file.path(dirname(input_file), "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
# Explicit NA markers distinguish missing values from reported zeros.
write.csv(data_tidy_all, file.path(output_dir, "godt_tidy_validated.csv"),
          row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
write.csv(data_quality_issues, file.path(output_dir, "data_quality_issues.csv"),
          row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
write.csv(validation_summary, file.path(output_dir, "validation_summary.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

print(validation_summary, n = Inf)
message("Step 1 complete. Data and validation results saved in: ", output_dir)
if (!run_analyses) {
  message("Existing analyses were not run (run_analyses = FALSE).")
}

# END OF IMPORT AND VALIDATION
# ============================================================
# ANALYSES
# ============================================================

if (run_analyses) {
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Install ggrepel to run the existing analyses.", call. = FALSE)
  }
  library(ggrepel)

# PLOT 1
dd = godt_clean[godt_clean$indicator == "total_actual_dd" & !is.na(godt_clean$value_pmp), ]

regional_dd = aggregate(value_pmp ~ region + year, data = dd, FUN = median, na.rm = TRUE)
names(regional_dd)[names(regional_dd) == "value_pmp"] = "median_dd_pmp"

p1 = ggplot(regional_dd, aes(x = year, y = median_dd_pmp, colour = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title    = "Median Deceased Donor Rate by Region (2000–2024)",
    subtitle = "Per million population (pmp); median across reporting countries",
    x = "Year", y = "Deceased donors pmp", colour = " Region"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p1)

#PLOT 2

dd2 = dd

# max year within each country
max_year_by_country = ave(dd2$year, dd2$country, FUN = max)

# keep only most recent year rows
recent = dd2[dd2$year == max_year_by_country, ]

# sort descending by value_pmp
recent = recent[order(-recent$value_pmp), ]

#top 15 
top_n = min(15, nrow(recent))
top15 = recent[1:top_n, ]

p2 = ggplot(top15, aes(x = reorder(country, value_pmp), y = value_pmp, fill = region)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 Countries: Deceased Donor Rate (Most Recent Year)",
    x = NULL, y = "Deceased donors per million population", fill = "Region"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p2)

# PLOT 3

# 0) rebuild dd 
dd <- godt_clean[
  godt_clean$indicator == "total_actual_dd" & !is.na(godt_clean$value_pmp),
]

# 1) keep years of interest
covid <- dd[dd$year %in% c(2019, 2020, 2021),
            c("country","region","year","value_pmp")]

# 2) ensuring one row per country-region-year (median if duplicates)
covid <- aggregate(value_pmp ~ country + region + year, data = covid,
                   FUN = median, na.rm = TRUE)

# 3) widen
covid_w <- reshape(covid,
                   idvar = c("country","region"),
                   timevar = "year",
                   direction = "wide")

# 4) keeping only complete 2019/2020/2021 reporters
need <- c("value_pmp.2019","value_pmp.2020","value_pmp.2021")
keep <- complete.cases(covid_w[, need])
covid_w <- covid_w[keep, ]

# 5) % change (note: *100 makes it percent)
covid_w <- covid_w[covid_w$value_pmp.2019 > 0, ]
covid_w$drop_2020 <- (covid_w$value_pmp.2020 - covid_w$value_pmp.2019) /
  covid_w$value_pmp.2019 * 100

# 6) plot
p3 <- ggplot(covid_w, aes(x = drop_2020)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red", linewidth = 1) +
  labs(
    title    = "COVID-19 Impact: Distribution of % Change in Deceased Donation (2019 → 2020)",
    subtitle = "Red dashed line = no change; most countries fell below zero",
    x = "% change in deceased donors pmp", y = "Number of countries"
  ) +
  theme_minimal(base_size = 11)

print(p3)

#° PLOT 4

ld_share <- godt_clean |>
  filter(organ == "kidney", indicator %in% c("ld_kidney_tx", "dd_kidney_tx")) |>
  select(region, country, year, indicator, value) |>
  pivot_wider(names_from = indicator, values_from = value) |>
  filter(!is.na(ld_kidney_tx), !is.na(dd_kidney_tx)) |>
  mutate(ld_share = ld_kidney_tx / (dd_kidney_tx + ld_kidney_tx)) |>
  group_by(region, year) |>
  summarise(mean_ld_share = mean(ld_share, na.rm = TRUE), .groups = "drop")
str(regional_dd$year)
p4 <- ggplot(ld_share, aes(x = year, y = mean_ld_share, colour = region)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Share of Living Donor Kidney Transplants by Region",
    subtitle = "High living-donor share signals weak deceased-donor infrastructure",
    x = "Year", y = "Living donor share of kidney transplants", colour = "Region"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p4)

# PLOT 5

spain = dd[dd$country == "Spain", ]

p5 = ggplot(spain, aes(x = year, y = value_pmp)) +
  geom_line(colour = "#C60B1E", linewidth = 1.3) +
  geom_point(colour = "#C60B1E", size = 2) +
  annotate("rect", xmin = 2019.5, xmax = 2020.5,
           ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "navy") +
  annotate("text", x = 2020.5, y = max(spain$value_pmp, na.rm = TRUE) * 0.88,
           label = "COVID-19\ndip", colour = "navy", size = 3.2, hjust = 0) +
  labs(
    title    = "Spain: Deceased Donor Rate 2000–2024",
    x = "Year", y = "Deceased donors per million population"
  ) +
  theme_minimal(base_size = 11)

print(p5)




# PLOT 6 A(Global): Worldwide Adoption of DCD Protocols


global_dcd_share <- godt_clean |>
  filter(indicator %in% c("actual_dbd", "actual_dcd")) |>
  select(year, indicator, value) |>
  # Sum of everything globally by year
  group_by(year, indicator) |>
  summarise(global_total = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = indicator, values_from = global_total) |>
  #% of total donors that come from DCD
  mutate(
    total_dd = actual_dbd + actual_dcd,
    dcd_share = actual_dcd / total_dd
  ) |>
  filter(year >= 2000 & year <= 2024)

p6_global <- ggplot(global_dcd_share, aes(x = year, y = dcd_share)) +
  geom_line(colour = "darkorange", linewidth = 1.5) +
  geom_point(colour = "darkorange", size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "The Global Evolution of Organ Donation Protocols",
    subtitle = "Percentage of deceased donors identified via Circulatory Death (DCD) worldwide",
    x = "Year", y = "DCD Share (%)"
  ) +
  theme_minimal(base_size = 11)

print(p6_global)





# Organ Yield Analysis (Efficiency Ratio)

#-------------------P------------7A------------------------------
# Who is good at finding donors vs. who is good at actually transplanting them?
#
efficiency_data <- godt_clean |>
  filter(indicator %in% c("total_actual_dd", "total_utilized_dd")) |>
  # Let's take the most recent year of data for each country
  group_by(country, indicator) |>
  filter(year == max(year)) |>
  ungroup() |>
  select(country, region, population_millions, indicator, value) |>
  # Pivot wider so actual and utilized are side-by-side columns
  pivot_wider(names_from = indicator, values_from = value) |>
  drop_na(total_actual_dd, total_utilized_dd, population_millions) |>
  mutate(
    donors_pmp = total_actual_dd / population_millions,
    # The Efficiency Ratio:
    efficiency_ratio = total_utilized_dd / total_actual_dd
  ) |>
  # Filter out countries with less than 10 total donors to avoid wild efficiency 
  # percentages driven by tiny sample sizes
  filter(total_actual_dd >= 10)

p7 <- ggplot(efficiency_data, aes(x = donors_pmp, y = efficiency_ratio, color = region)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text_repel(aes(label = country), size = 3, max.overlaps = 12, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  # Add a 100% reference line (the theoretical maximum)
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) + 
  labs(
    title = "System Efficiency: Utilization vs. Identification Rate",
    subtitle = "Countries above 90% efficiently transplant almost all identified donors",
    x = "Deceased Donors Per Million Population (Identification Rate)",
    y = "Utilization Efficiency (Utilized / Actual)",
    color = "Region"
  ) +
  theme_minimal(base_size = 11)

print(p7)

#----------------------P--------------7B-----------------------

#calculate efficiency as ratio of total_dd and total_utolized_dd
efficiency <- data_tidy %>%
  filter(indicator %in% c("total_actual_dd", "total_utilized_dd")) %>%
  group_by(year, indicator) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  mutate(
    rate = ifelse(total_actual_dd > 0,
                  total_utilized_dd / total_actual_dd,
                  NA_real_)
  ) %>%
  filter(!is.na(rate), !is.infinite(rate))
#ggplot the efficiency calculated before
ggplot(efficiency, aes(x = year, y = rate)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  labs(
    title = "Global efficiency (DD)",
    x = "Year",
    y = "Utilization rate"
  ) +
  theme_minimal()

#--------------------P--------------8-------------------

#calculate possible reason for 2018 shock
eff_country <- data_tidy %>%
  filter(indicator %in% c("total_actual_dd", "total_utilized_dd")) %>%
  select(region, country, year, indicator, value) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  mutate(rate = total_utilized_dd / total_actual_dd)

missing_utilized <- eff_country %>%
  filter(year >= 2013,
         !is.na(total_actual_dd),
         total_actual_dd > 0,
         is.na(total_utilized_dd)) %>%
  arrange(desc(total_actual_dd))
#plot table 
missing_utilized


#-------------------------P-------9---------------------

#calculate dbd vs dcd 
dbd_dcd <- data_tidy %>%
  filter(indicator %in% c("actual_dbd", "actual_dcd")) %>%
  group_by(year, indicator) %>%
  summarise(total = sum(value, na.rm = TRUE))
#plot it
ggplot(dbd_dcd, aes(x = year, y = total, color = indicator)) +
  geom_line(linewidth= 1.2) +
  labs(title = "DBD vs DCD ")

#-----------DIAGNOSIS--------
#--------------------------P-----1---------------------------
# CHANGE IN COUNTRIES RECORDED?
dd_cov <- godt_clean |>
  filter(indicator == "total_actual_dd") |>
  group_by(region, year) |>
  summarise(
    n_value = n_distinct(country[!is.na(value)]),
    n_pop   = n_distinct(country[!is.na(population_millions) & population_millions > 0]),
    n_pmp   = n_distinct(country[!is.na(value_pmp)]),
    .groups = "drop"
  )

dd_cov |>
  filter(region %in% c("America", "Western Pacific"),
         year %in% 2000:2010) |>
  arrange(region, year)

#WHICH COUNTRIES?
dd_country_year <- godt_clean |>
  filter(indicator == "total_actual_dd", !is.na(value_pmp)) |>
  distinct(region, country, year)

# America: countries present in 2003 but not 2004
lost_America <- anti_join(
  dd_country_year |> filter(region=="America", year==2003),
  dd_country_year |> filter(region=="America", year==2004),
  by = c("region","country")
) |> arrange(country)

# America: countries present in 2004 but not 2003
gained_America <- anti_join(
  dd_country_year |> filter(region=="America", year==2004),
  dd_country_year |> filter(region=="America", year==2003),
  by = c("region","country")
) |> arrange(country)

lost_America
gained_America

#DOES DROP REMAIN WITH SAME COUNTRIES?

common_America <- inner_join(
  dd_country_year |> filter(region=="America", year==2003) |> distinct(country),
  dd_country_year |> filter(region=="America", year==2004) |> distinct(country),
  by="country"
)

dd_common_check <- godt_clean |>
  filter(region=="America",
         indicator=="total_actual_dd",
         year %in% c(2003,2004),
         !is.na(value_pmp)) |>
  semi_join(common_America, by="country") |>
  group_by(year) |>
  summarise(
    n_countries = n_distinct(country),
    median_pmp  = median(value_pmp, na.rm=TRUE),
    .groups="drop"
  )

dd_common_check
# ANSWER: NO IT INCREASES( 17.8-->18.6)





# ============================================================
# ADDITIONAL PERSONAL ANALYSIS
# Original group-analysis sections above are retained.
# The import/validation block was revised separately for the portfolio.
# These sections build on the objects already created.
# ============================================================


#----------------------P--------------3B-----------------------
# COVID RECOVERY IN 2021
# PLOT 3 measured the 2019 -> 2020 fall. Here I also use the
# 2021 values already present in covid_w to measure the recovery.

covid_recovery <- covid_w[covid_w$value_pmp.2020 > 0, ]

# % recovery from 2020 to 2021
covid_recovery$rec_2021 <- (covid_recovery$value_pmp.2021 - covid_recovery$value_pmp.2020) /
  covid_recovery$value_pmp.2020 * 100

# 2021 level compared with the pre-COVID 2019 level
covid_recovery$gap_2021_vs_2019 <- (covid_recovery$value_pmp.2021 - covid_recovery$value_pmp.2019) /
  covid_recovery$value_pmp.2019 * 100

p3b <- ggplot(covid_recovery,
              aes(x = drop_2020, y = gap_2021_vs_2019, colour = region)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  labs(
    title = "COVID-19 Shock and 2021 Recovery in Deceased Donation",
    subtitle = "Below 0 on the y-axis means 2021 remained below the country's 2019 rate",
    x = "% change in deceased donors pmp (2019 -> 2020)",
    y = "% difference in deceased donors pmp (2021 vs 2019)",
    colour = "Region"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p3b)

# regional summary of the COVID shock and recovery
covid_region_summary <- covid_recovery |>
  group_by(region) |>
  summarise(
    n_countries = n_distinct(country),
    median_drop_2020 = median(drop_2020, na.rm = TRUE),
    median_recovery_2021 = median(rec_2021, na.rm = TRUE),
    median_gap_2021_vs_2019 = median(gap_2021_vs_2019, na.rm = TRUE),
    .groups = "drop"
  )

covid_region_summary


#----------------------P--------------6B-----------------------
# DCD ADOPTION BY REGION
# PLOT 6A looked at the global DCD share. Here I calculate the
# same quantity by region, using only country-years where both
# DBD and DCD values are available.

dcd_country <- godt_clean |>
  filter(indicator %in% c("actual_dbd", "actual_dcd")) |>
  select(country, region, year, indicator, value) |>
  group_by(country, region, year, indicator) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = indicator, values_from = value) |>
  filter(!is.na(actual_dbd), !is.na(actual_dcd),
         actual_dbd + actual_dcd > 0)

regional_dcd_share <- dcd_country |>
  group_by(region, year) |>
  summarise(
    dbd_total = sum(actual_dbd, na.rm = TRUE),
    dcd_total = sum(actual_dcd, na.rm = TRUE),
    n_countries = n_distinct(country),
    .groups = "drop"
  ) |>
  mutate(
    total_dd = dbd_total + dcd_total,
    dcd_share = dcd_total / total_dd
  ) |>
  filter(year >= 2000 & year <= 2024)

p6_region <- ggplot(regional_dcd_share,
                    aes(x = year, y = dcd_share, colour = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Regional Adoption of DCD Protocols",
    subtitle = "DCD donors as a share of DBD + DCD donors among countries reporting both measures",
    x = "Year", y = "DCD Share (%)", colour = "Region"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p6_region)


#----------------------P--------------7C-----------------------
# COUNTRY-LEVEL UTILIZATION EFFICIENCY OVER TIME
# PLOT 7B uses the ratio of global totals. That ratio can change
# when the set of reporting countries changes. Here efficiency is
# first calculated country by country and then summarized by year.

eff_country_complete <- godt_clean |>
  filter(indicator %in% c("total_actual_dd", "total_utilized_dd")) |>
  select(country, region, year, indicator, value) |>
  group_by(country, region, year, indicator) |>
  summarise(value = median(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = indicator, values_from = value) |>
  filter(!is.na(total_actual_dd), !is.na(total_utilized_dd),
         total_actual_dd >= 10) |>
  mutate(rate = total_utilized_dd / total_actual_dd) |>
  filter(!is.na(rate), !is.infinite(rate))

efficiency_country_year <- eff_country_complete |>
  group_by(year) |>
  summarise(
    median_rate = median(rate, na.rm = TRUE),
    mean_rate = mean(rate, na.rm = TRUE),
    n_countries = n_distinct(country),
    .groups = "drop"
  )

p7_country <- ggplot(efficiency_country_year,
                     aes(x = year, y = median_rate)) +
  geom_line(linewidth = 1.2, colour = "steelblue") +
  geom_point(size = 1.5, colour = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Median Country-Level Deceased Donor Utilization Efficiency",
    subtitle = "Each country contributes one utilization ratio per year before the median is calculated",
    x = "Year", y = "Median utilization rate"
  ) +
  theme_minimal(base_size = 11)

print(p7_country)


#----------------------P--------------8B-----------------------
# REPORTING COVERAGE FOR UTILIZED DONORS
# This extends the 2018-shock diagnosis by quantifying how many
# countries report actual donors and how many also report utilized donors.

eff_reporting <- godt_clean |>
  filter(indicator %in% c("total_actual_dd", "total_utilized_dd")) |>
  distinct(country, region, year, indicator) |>
  mutate(reported = TRUE) |>
  pivot_wider(
    names_from = indicator,
    values_from = reported,
    values_fill = FALSE
  )

eff_coverage <- eff_reporting |>
  group_by(year) |>
  summarise(
    n_actual = sum(total_actual_dd),
    n_utilized = sum(total_utilized_dd),
    n_both = sum(total_actual_dd & total_utilized_dd),
    .groups = "drop"
  ) |>
  mutate(
    utilized_coverage = ifelse(n_actual > 0, n_both / n_actual, NA_real_)
  )

p8_coverage <- ggplot(eff_coverage, aes(x = year, y = utilized_coverage)) +
  geom_line(linewidth = 1.2, colour = "darkgreen") +
  geom_point(size = 1.5, colour = "darkgreen") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Reporting Coverage for Utilized Deceased Donors",
    subtitle = "Share of countries reporting actual donors that also report utilized donors",
    x = "Year", y = "Reporting coverage"
  ) +
  theme_minimal(base_size = 11)

print(p8_coverage)

eff_coverage |>
  filter(year >= 2013) |>
  arrange(year)


#-------------------------P-------9B----------------------------
# DBD VS DCD AS SHARES RATHER THAN RAW TOTALS
# PLOT 9 uses raw global counts, which are influenced by the
# number and size of reporting countries. This version focuses on
# the composition of reported deceased donation.

dbd_dcd_share <- dbd_dcd |>
  pivot_wider(names_from = indicator, values_from = total) |>
  mutate(
    total_dd = actual_dbd + actual_dcd,
    dbd_share = actual_dbd / total_dd,
    dcd_share = actual_dcd / total_dd
  ) |>
  select(year, dbd_share, dcd_share) |>
  pivot_longer(
    cols = c(dbd_share, dcd_share),
    names_to = "donor_protocol",
    values_to = "share"
  )

p9b <- ggplot(dbd_dcd_share,
              aes(x = year, y = share, colour = donor_protocol)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "DBD vs DCD Composition of Reported Deceased Donation",
    x = "Year", y = "Share of DBD + DCD donors", colour = "Protocol"
  ) +
  theme_minimal(base_size = 11)

print(p9b)


#-----------TIDY DATA / DATA QUALITY CHECKS--------------------
# These checks follow the tidy-data logic used to construct
# godt_tidy_clean: one row should represent one country-year-indicator
# observation, and population should be consistent within country-year.


#--------------------------CHECK-----1--------------------------
# NUMBER OF REPORTING COUNTRIES OVER TIME FOR TOTAL ACTUAL DD
# This generalizes the America 2003/2004 diagnosis already above.

reporting_dd_year <- godt_clean |>
  filter(indicator == "total_actual_dd", !is.na(value_pmp)) |>
  group_by(year) |>
  summarise(
    n_countries = n_distinct(country),
    .groups = "drop"
  )

p_reporting <- ggplot(reporting_dd_year,
                      aes(x = year, y = n_countries)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  labs(
    title = "Number of Countries Reporting Deceased Donor Data",
    subtitle = "Changes in coverage can create apparent changes in global or regional aggregates",
    x = "Year", y = "Number of reporting countries"
  ) +
  theme_minimal(base_size = 11)

print(p_reporting)


#--------------------------CHECK-----5--------------------------
# REGIONAL REPORTING COVERAGE OVER TIME

reporting_dd_region <- godt_clean |>
  filter(indicator == "total_actual_dd", !is.na(value_pmp)) |>
  group_by(region, year) |>
  summarise(
    n_countries = n_distinct(country),
    .groups = "drop"
  )

p_reporting_region <- ggplot(reporting_dd_region,
                             aes(x = year, y = n_countries, colour = region)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Regional Reporting Coverage for Deceased Donor Data",
    x = "Year", y = "Number of reporting countries", colour = "Region"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p_reporting_region)

# ============================================================
# SAVE PORTFOLIO FIGURES
# ============================================================

figures_dir <- file.path(output_dir, "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(figures_dir, "01_regional_deceased_donor_rates.png"),
  plot = p1,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "02_covid_shock_and_recovery.png"),
  plot = p3b,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "03_regional_dcd_share.png"),
  plot = p6_region,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "04_utilization_efficiency.png"),
  plot = p7_country,
  width = 9,
  height = 6,
  dpi = 300
)

message("Portfolio figures saved in: ", figures_dir)

}
