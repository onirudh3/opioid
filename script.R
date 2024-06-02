
# Libraries ---------------------------------------------------------------

library(dplyr)
library(readxl)
library(did)
library(ggplot2)


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


# Prescription Policy Data ------------------------------------------------

# Merge to df
df <- left_join(df, read_excel("Data/prescription_policies_dates.xlsx"))


# Event study -------------------------------------------------------------

out <- att_gt(yname = "Deaths",
              gname = "MedicaidPolicyDate",
              idname = "State_ID",
              tname = "YearMonth",
              data = df, alp = 0.1)

# Overall average treatment effect
summary(aggte(out, type = "group"))

# Event study plot 
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  theme_classic(base_size = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
