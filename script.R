
# Libraries ---------------------------------------------------------------

library(dplyr)


# NCHS Overdose Data ------------------------------------------------------

# Load data
df <- read.csv("Data/VSRR_Provisional_Drug_Overdose_Death_Counts_20240528.csv")

# We need only overdose deaths
df <- df %>% subset(Indicator == "Number of Drug Overdose Deaths")

# Merging year and month columns
df <- df %>%
  mutate(YearMonth = as.integer(paste0(Year, sprintf("%02d", match(Month, month.name)))),
         .after = Month)

# Rename Data.Value to Deaths
df <- df %>% rename("Deaths" = "Data.Value")

# Arrange YearMonth by State
df <- df %>% group_by(State) %>% arrange(State, YearMonth)

# Select necessary columns
df <- subset(df, select = c(State, YearMonth, Deaths))

# Remove rows for United States and New York City which are not states
df <- subset(df, !(State %in% c("US", "YC")))


# Prescription Policy Data ------------------------------------------------

# Column for existing state policy


