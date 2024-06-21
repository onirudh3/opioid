# Anirudh Ravishankar
# June 2024

# Libraries ---------------------------------------------------------------

library(dplyr)
library(readxl)
library(did)
library(ggplot2)
library(stargazer)
library(missForest)


# NCHS Overdose Data ------------------------------------------------------

# Load data
df <- read.csv("Data/VSRR_Provisional_Drug_Overdose_Death_Counts_20240528.csv")

# We need only overdose deaths
df <- df %>% subset(Indicator == "Number of Drug Overdose Deaths")

# 12 month ending period in December is the value for the year
df <- df %>% subset(Month == "December")

# Remove rows for United States and New York City which are not states
df <- subset(df, !(State.Name %in% c("United States", "New York City")))

# Select necessary columns
df <- subset(df, select = c(State.Name, Year, Data.Value))

# Rename Data.Value to Deaths
df <- df %>% rename("Deaths" = "Data.Value")

# Assign ID to each state
df <- df %>% 
  group_by(State.Name) %>% 
  mutate(State_ID = cur_group_id()) %>% 
  arrange(State_ID)


# Auxiliary Data and Processing -------------------------------------------

# Merge to df
df <- left_join(df, read_excel("Data/auxiliary_data.xlsx"))

# Average physician density from years 2009, 2019
df <- df %>% 
  mutate(PhysicianDensity = (PhysicianDensity2009 + PhysicianDensity2019) / 2, 
         .after = PhysicianDensity2019)

# Average population from years 2010, 2020, 2022
df <- df %>% 
  mutate(Pop = (Pop2010 + Pop2020 + Pop2022) / 3)

# Presence of medicaid policy 
df <- df %>% 
  mutate(MedicaidPolicy = case_when(MedicaidPolicyDate == 0 ~ 0,
                                    T ~ 1), .after = MedicaidPolicyDate)

# Log deaths
df <- df %>% mutate(LogDeaths = log(Deaths), .after = Deaths)


# Opioid Prescribing Rate Data --------------------------------------------

# Read data
dx <- read.csv("Data/medicaid_opioid_prescribing_rates.csv")

# Plan_Type = "All"
dx <- subset(dx, Plan_Type == "All")

# Select columns
dx <- subset(dx, select = c(Geo_Desc, Year, Opioid_Prscrbng_Rate))

# Rename stuff
dx <- dx %>% rename("State.Name" = "Geo_Desc", "OpioidPrescribingRate" = "Opioid_Prscrbng_Rate")

# Merge to df
df <- left_join(df, dx)

# Impute missing data using random forest
set.seed(123)
df <- cbind(df[, 1], missForest(data.frame(df)[, 2:21])[["ximp"]])

# Average across the years
df <- df %>% group_by(State_ID) %>% mutate(OpioidPrescribingRate = mean(OpioidPrescribingRate))
df$OpioidPrescribingRate <- round(df$OpioidPrescribingRate, 3)


# Summary Statistics ------------------------------------------------------

# stargazer(data.frame(df))


# Event study -------------------------------------------------------------

out <- att_gt(yname = "LogDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              xformla = ~ExistingPolicy + ExistingPDMP + MedicaidPolicy + PhysicianDensity + OpioidPrescribingRate,
              # control_group = "notyettreated",
              data = df,
              alp = 0.05)

# Overall average treatment effect
summary(aggte(out, type = "group", na.rm = T))

# Event study plot
pdf(file = "Figures/eventstudy.pdf", height = 4, width = 6)
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  theme_classic(base_size = 12) +
  ylim(-0.7, 0.7)
dev.off()










