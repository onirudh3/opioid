
# Libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readxl)
library(missForest)


# Data --------------------------------------------------------------------

# ICD-10 codes for opioid deaths
codes <- c("T40.0", "T40.1", "T40.2", "T40.3", "T40.4", "T40.5", "T40.6")

# 2015
df <- read.csv("Data/Deaths by Age Group/deaths_2015.txt", sep = "\t")
df <- subset(df, Multiple.Cause.of.death.Code %in% codes)
df <- df %>% 
  group_by(State, Ten.Year.Age.Groups.Code) %>% 
  summarise(Deaths = sum(Deaths))
df$Year <- 2015

# 2016
df1 <- read.csv("Data/Deaths by Age Group/deaths_2016.txt", sep = "\t")
df1 <- subset(df1, Multiple.Cause.of.death.Code %in% codes)
df1 <- df1 %>% 
  group_by(State, Ten.Year.Age.Groups.Code) %>% 
  summarise(Deaths = sum(Deaths))
df1$Year <- 2016

# 2017
df2 <- read.csv("Data/Deaths by Age Group/deaths_2017.txt", sep = "\t")
df2 <- subset(df2, Multiple.Cause.of.death.Code %in% codes)
df2 <- df2 %>% 
  group_by(State, Ten.Year.Age.Groups.Code) %>% 
  summarise(Deaths = sum(Deaths))
df2$Year <- 2017

# 2018
df3 <- read.csv("Data/Deaths by Age Group/deaths_2018.txt", sep = "\t")
df3 <- subset(df3, Multiple.Cause.of.death.Code %in% codes)
df3 <- df3 %>% 
  group_by(State, Ten.Year.Age.Groups.Code) %>% 
  summarise(Deaths = sum(Deaths))
df3$Year <- 2018

# 2019
df4 <- read.csv("Data/Deaths by Age Group/deaths_2019.txt", sep = "\t")
df4 <- subset(df4, Multiple.Cause.of.death.Code %in% codes)
df4 <- df4 %>% 
  group_by(State, Ten.Year.Age.Groups.Code) %>% 
  summarise(Deaths = sum(Deaths))
df4$Year <- 2019

# 2020
df5 <- read.csv("Data/Deaths by Age Group/deaths_2020.txt", sep = "\t")
df5 <- subset(df5, Multiple.Cause.of.death.Code %in% codes)
df5 <- df5 %>% 
  group_by(State, Ten.Year.Age.Groups.Code) %>% 
  summarise(Deaths = sum(Deaths))
df5$Year <- 2020

# 2021
df6 <- read.csv("Data/Deaths by Age Group/deaths_2021.txt", sep = "\t")
df6 <- subset(df6, Multiple.Cause.of.death.Code %in% codes)
df6 <- df6 %>% 
  group_by(State, Ten.Year.Age.Groups.Code) %>% 
  summarise(Deaths = sum(Deaths))
df6$Year <- 2021

# 2022
df7 <- read.csv("Data/Deaths by Age Group/deaths_2022.txt", sep = "\t")
df7 <- subset(df7, Multiple.Cause.of.death.Code %in% codes)
df7 <- df7 %>% 
  group_by(State, Ten.Year.Age.Groups.Code) %>% 
  summarise(Deaths = sum(Deaths))
df7$Year <- 2022


# Bind rows ---------------------------------------------------------------

df <- bind_rows(df, df1, df2, df3, df4, df5, df6, df7)
rm(df1, df2, df3, df4, df5, df6, df7, codes)
df <- df %>% 
  mutate(Ten.Year.Age.Groups.Code = case_when(Ten.Year.Age.Groups.Code == "85+" ~ "75-84", 
                                               T ~ Ten.Year.Age.Groups.Code))
df <- df %>% 
  mutate(Ten.Year.Age.Groups.Code = case_when(Ten.Year.Age.Groups.Code == "75-84" ~ "75+", 
                                              T ~ Ten.Year.Age.Groups.Code))
df <- df %>% 
  group_by(State, Year, Ten.Year.Age.Groups.Code) %>% 
  mutate(Deaths = sum(Deaths)) %>% 
  distinct()
df <- df %>% rename("State.Name" = "State")

# 15-24
dx <- subset(df, Ten.Year.Age.Groups.Code == "15-24")
dx <- subset(dx, select = -c(Ten.Year.Age.Groups.Code))
dx <- dx %>%
  ungroup() %>% 
  complete(State.Name, Year) %>% 
  replace(is.na(.), 0) # Balance panel
dx <- dx %>% 
  group_by(State.Name) %>% 
  mutate(State_ID = cur_group_id()) %>% 
  arrange(State_ID) # Assign ID to each state
dx$LogDeaths <- log(dx$Deaths + 1)
dx <- left_join(dx, read_excel("Data/auxiliary_data.xlsx"))
dx <- dx %>% 
  mutate(MedicaidPolicy = case_when(MedicaidPolicyDate == 0 ~ 0,
                                    T ~ 1), .after = MedicaidPolicyDate)
dx <- dx %>% 
  mutate(PhysicianDensity = (PhysicianDensity2009 + PhysicianDensity2019) / 2, 
         .after = PhysicianDensity2019)
dx <- dx %>% mutate(LogAvgTemp = log(AvgTemp), .after = AvgTemp)
dz <- read.csv("Data/medicaid_opioid_prescribing_rates.csv")
dz <- subset(dz, Plan_Type == "All")
dz <- subset(dz, select = c(Geo_Desc, Year, Opioid_Prscrbng_Rate))
dz <- dz %>% rename("State.Name" = "Geo_Desc", "OpioidPrescribingRate" = "Opioid_Prscrbng_Rate")
dx <- left_join(dx, dz)
set.seed(1)
dx$PoliticalLeaning <- as.factor(dx$PoliticalLeaning)
dx <- cbind(dx[, 1], missForest(data.frame(dx)[, 2:27])[["ximp"]])
dx <- dx %>% group_by(State_ID) %>% mutate(OpioidPrescribingRate = mean(OpioidPrescribingRate))
dx$OpioidPrescribingRate <- round(dx$OpioidPrescribingRate, 3)
write.csv(dx, file = "Data/Deaths by Age Group/deaths_15_to_24.csv", row.names = F)

# 25-34
dx <- subset(df, Ten.Year.Age.Groups.Code == "25-34")
dx <- subset(dx, select = -c(Ten.Year.Age.Groups.Code))
dx <- dx %>%
  ungroup() %>% 
  complete(State.Name, Year) %>% 
  replace(is.na(.), 0)
dx <- dx %>% 
  group_by(State.Name) %>% 
  mutate(State_ID = cur_group_id()) %>% 
  arrange(State_ID)
dx$LogDeaths <- log(dx$Deaths + 1)
dx <- left_join(dx, read_excel("Data/auxiliary_data.xlsx"))
dx <- dx %>% 
  mutate(MedicaidPolicy = case_when(MedicaidPolicyDate == 0 ~ 0,
                                    T ~ 1), .after = MedicaidPolicyDate)
dx <- dx %>% 
  mutate(PhysicianDensity = (PhysicianDensity2009 + PhysicianDensity2019) / 2, 
         .after = PhysicianDensity2019)
dx <- dx %>% mutate(LogAvgTemp = log(AvgTemp), .after = AvgTemp)
dz <- read.csv("Data/medicaid_opioid_prescribing_rates.csv")
dz <- subset(dz, Plan_Type == "All")
dz <- subset(dz, select = c(Geo_Desc, Year, Opioid_Prscrbng_Rate))
dz <- dz %>% rename("State.Name" = "Geo_Desc", "OpioidPrescribingRate" = "Opioid_Prscrbng_Rate")
dx <- left_join(dx, dz)
set.seed(1)
dx$PoliticalLeaning <- as.factor(dx$PoliticalLeaning)
dx <- cbind(dx[, 1], missForest(data.frame(dx)[, 2:27])[["ximp"]])
dx <- dx %>% group_by(State_ID) %>% mutate(OpioidPrescribingRate = mean(OpioidPrescribingRate))
dx$OpioidPrescribingRate <- round(dx$OpioidPrescribingRate, 3)
write.csv(dx, file = "Data/Deaths by Age Group/deaths_25_to_34.csv", row.names = F)

# 35-44
dx <- subset(df, Ten.Year.Age.Groups.Code == "35-44")
dx <- subset(dx, select = -c(Ten.Year.Age.Groups.Code))
dx <- dx %>%
  ungroup() %>% 
  complete(State.Name, Year) %>% 
  replace(is.na(.), 0)
dx <- dx %>% 
  group_by(State.Name) %>% 
  mutate(State_ID = cur_group_id()) %>% 
  arrange(State_ID)
dx$LogDeaths <- log(dx$Deaths + 1)
dx <- left_join(dx, read_excel("Data/auxiliary_data.xlsx"))
dx <- dx %>% 
  mutate(MedicaidPolicy = case_when(MedicaidPolicyDate == 0 ~ 0,
                                    T ~ 1), .after = MedicaidPolicyDate)
dx <- dx %>% 
  mutate(PhysicianDensity = (PhysicianDensity2009 + PhysicianDensity2019) / 2, 
         .after = PhysicianDensity2019)
dx <- dx %>% mutate(LogAvgTemp = log(AvgTemp), .after = AvgTemp)
dz <- read.csv("Data/medicaid_opioid_prescribing_rates.csv")
dz <- subset(dz, Plan_Type == "All")
dz <- subset(dz, select = c(Geo_Desc, Year, Opioid_Prscrbng_Rate))
dz <- dz %>% rename("State.Name" = "Geo_Desc", "OpioidPrescribingRate" = "Opioid_Prscrbng_Rate")
dx <- left_join(dx, dz)
set.seed(1)
dx$PoliticalLeaning <- as.factor(dx$PoliticalLeaning)
dx <- cbind(dx[, 1], missForest(data.frame(dx)[, 2:27])[["ximp"]])
dx <- dx %>% group_by(State_ID) %>% mutate(OpioidPrescribingRate = mean(OpioidPrescribingRate))
dx$OpioidPrescribingRate <- round(dx$OpioidPrescribingRate, 3)
write.csv(dx, file = "Data/Deaths by Age Group/deaths_35_to_44.csv", row.names = F)

# 45-54
dx <- subset(df, Ten.Year.Age.Groups.Code == "45-54")
dx <- subset(dx, select = -c(Ten.Year.Age.Groups.Code))
dx <- dx %>%
  ungroup() %>% 
  complete(State.Name, Year) %>% 
  replace(is.na(.), 0)
dx <- dx %>% 
  group_by(State.Name) %>% 
  mutate(State_ID = cur_group_id()) %>% 
  arrange(State_ID)
dx$LogDeaths <- log(dx$Deaths + 1)
dx <- left_join(dx, read_excel("Data/auxiliary_data.xlsx"))
dx <- dx %>% 
  mutate(MedicaidPolicy = case_when(MedicaidPolicyDate == 0 ~ 0,
                                    T ~ 1), .after = MedicaidPolicyDate)
dx <- dx %>% 
  mutate(PhysicianDensity = (PhysicianDensity2009 + PhysicianDensity2019) / 2, 
         .after = PhysicianDensity2019)
dx <- dx %>% mutate(LogAvgTemp = log(AvgTemp), .after = AvgTemp)
dz <- read.csv("Data/medicaid_opioid_prescribing_rates.csv")
dz <- subset(dz, Plan_Type == "All")
dz <- subset(dz, select = c(Geo_Desc, Year, Opioid_Prscrbng_Rate))
dz <- dz %>% rename("State.Name" = "Geo_Desc", "OpioidPrescribingRate" = "Opioid_Prscrbng_Rate")
dx <- left_join(dx, dz)
set.seed(1)
dx$PoliticalLeaning <- as.factor(dx$PoliticalLeaning)
dx <- cbind(dx[, 1], missForest(data.frame(dx)[, 2:27])[["ximp"]])
dx <- dx %>% group_by(State_ID) %>% mutate(OpioidPrescribingRate = mean(OpioidPrescribingRate))
dx$OpioidPrescribingRate <- round(dx$OpioidPrescribingRate, 3)
write.csv(dx, file = "Data/Deaths by Age Group/deaths_45_to_54.csv", row.names = F)

# 55-64
dx <- subset(df, Ten.Year.Age.Groups.Code == "55-64")
dx <- subset(dx, select = -c(Ten.Year.Age.Groups.Code))
dx <- dx %>%
  ungroup() %>% 
  complete(State.Name, Year) %>% 
  replace(is.na(.), 0)
dx <- dx %>% 
  group_by(State.Name) %>% 
  mutate(State_ID = cur_group_id()) %>% 
  arrange(State_ID)
dx$LogDeaths <- log(dx$Deaths + 1)
dx <- left_join(dx, read_excel("Data/auxiliary_data.xlsx"))
dx <- dx %>% 
  mutate(MedicaidPolicy = case_when(MedicaidPolicyDate == 0 ~ 0,
                                    T ~ 1), .after = MedicaidPolicyDate)
dx <- dx %>% 
  mutate(PhysicianDensity = (PhysicianDensity2009 + PhysicianDensity2019) / 2, 
         .after = PhysicianDensity2019)
dx <- dx %>% mutate(LogAvgTemp = log(AvgTemp), .after = AvgTemp)
dz <- read.csv("Data/medicaid_opioid_prescribing_rates.csv")
dz <- subset(dz, Plan_Type == "All")
dz <- subset(dz, select = c(Geo_Desc, Year, Opioid_Prscrbng_Rate))
dz <- dz %>% rename("State.Name" = "Geo_Desc", "OpioidPrescribingRate" = "Opioid_Prscrbng_Rate")
dx <- left_join(dx, dz)
set.seed(1)
dx$PoliticalLeaning <- as.factor(dx$PoliticalLeaning)
dx <- cbind(dx[, 1], missForest(data.frame(dx)[, 2:27])[["ximp"]])
dx <- dx %>% group_by(State_ID) %>% mutate(OpioidPrescribingRate = mean(OpioidPrescribingRate))
dx$OpioidPrescribingRate <- round(dx$OpioidPrescribingRate, 3)
write.csv(dx, file = "Data/Deaths by Age Group/deaths_55_to_64.csv", row.names = F)

# 65-74
dx <- subset(df, Ten.Year.Age.Groups.Code == "65-74")
dx <- subset(dx, select = -c(Ten.Year.Age.Groups.Code))
dx <- dx %>%
  ungroup() %>% 
  complete(State.Name, Year) %>% 
  replace(is.na(.), 0)
dx <- dx %>% 
  group_by(State.Name) %>% 
  mutate(State_ID = cur_group_id()) %>% 
  arrange(State_ID)
dx$LogDeaths <- log(dx$Deaths + 1)
dx <- left_join(dx, read_excel("Data/auxiliary_data.xlsx"))
dx <- dx %>% 
  mutate(MedicaidPolicy = case_when(MedicaidPolicyDate == 0 ~ 0,
                                    T ~ 1), .after = MedicaidPolicyDate)
dx <- dx %>% 
  mutate(PhysicianDensity = (PhysicianDensity2009 + PhysicianDensity2019) / 2, 
         .after = PhysicianDensity2019)
dx <- dx %>% mutate(LogAvgTemp = log(AvgTemp), .after = AvgTemp)
dz <- read.csv("Data/medicaid_opioid_prescribing_rates.csv")
dz <- subset(dz, Plan_Type == "All")
dz <- subset(dz, select = c(Geo_Desc, Year, Opioid_Prscrbng_Rate))
dz <- dz %>% rename("State.Name" = "Geo_Desc", "OpioidPrescribingRate" = "Opioid_Prscrbng_Rate")
dx <- left_join(dx, dz)
set.seed(1)
dx$PoliticalLeaning <- as.factor(dx$PoliticalLeaning)
dx <- cbind(dx[, 1], missForest(data.frame(dx)[, 2:27])[["ximp"]])
dx <- dx %>% group_by(State_ID) %>% mutate(OpioidPrescribingRate = mean(OpioidPrescribingRate))
dx$OpioidPrescribingRate <- round(dx$OpioidPrescribingRate, 3)
write.csv(dx, file = "Data/Deaths by Age Group/deaths_65_to_74.csv", row.names = F)

# 75+
dx <- subset(df, Ten.Year.Age.Groups.Code == "75+")
dx <- subset(dx, select = -c(Ten.Year.Age.Groups.Code))
dx <- dx %>%
  ungroup() %>% 
  complete(State.Name, Year) %>% 
  replace(is.na(.), 0)
dx <- dx %>% 
  group_by(State.Name) %>% 
  mutate(State_ID = cur_group_id()) %>% 
  arrange(State_ID)
dx$LogDeaths <- log(dx$Deaths + 1)
dx <- left_join(dx, read_excel("Data/auxiliary_data.xlsx"))
dx <- dx %>% 
  mutate(MedicaidPolicy = case_when(MedicaidPolicyDate == 0 ~ 0,
                                    T ~ 1), .after = MedicaidPolicyDate)
dx <- dx %>% 
  mutate(PhysicianDensity = (PhysicianDensity2009 + PhysicianDensity2019) / 2, 
         .after = PhysicianDensity2019)
dx <- dx %>% mutate(LogAvgTemp = log(AvgTemp), .after = AvgTemp)
dz <- read.csv("Data/medicaid_opioid_prescribing_rates.csv")
dz <- subset(dz, Plan_Type == "All")
dz <- subset(dz, select = c(Geo_Desc, Year, Opioid_Prscrbng_Rate))
dz <- dz %>% rename("State.Name" = "Geo_Desc", "OpioidPrescribingRate" = "Opioid_Prscrbng_Rate")
dx <- left_join(dx, dz)
set.seed(1)
dx$PoliticalLeaning <- as.factor(dx$PoliticalLeaning)
dx <- cbind(dx[, 1], missForest(data.frame(dx)[, 2:27])[["ximp"]])
dx <- dx %>% group_by(State_ID) %>% mutate(OpioidPrescribingRate = mean(OpioidPrescribingRate))
dx$OpioidPrescribingRate <- round(dx$OpioidPrescribingRate, 3)
write.csv(dx, file = "Data/Deaths by Age Group/deaths_75_plus.csv", row.names = F)




