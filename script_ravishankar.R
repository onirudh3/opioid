# Anirudh Ravishankar
# July 2024


# Libraries ---------------------------------------------------------------

library(dplyr)
library(readxl)
library(did)
library(ggplot2)
library(stargazer)
library(missForest)
library(ggpubr)

# For maps
# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)
library(sf)

# Forbid scientific notation
options(scipen = 999)


# NCHS Overdose Data ------------------------------------------------------

# Load data
df <- read.csv("Data/VSRR_Provisional_Drug_Overdose_Death_Counts_20240528.csv")

# Remove rows for United States and New York City which are not states
df <- subset(df, !(State.Name %in% c("United States", "New York City")))

# 12 month ending period in December is the value for the year
df <- df %>% subset(Month == "December")

# Select necessary columns
df <- subset(df, select = c(State.Name, Year, Indicator, Data.Value))

# Create a new dataframe to re-merge because we want two columns for each indicator category
ds <- df %>% subset(Indicator %in% c("Number of Deaths"))
ds <- ds %>% rename("TotalDeaths" = "Data.Value")

# We need overdose deaths and total deaths
df <- df %>% subset(Indicator %in% c("Number of Drug Overdose Deaths"))
df <- df %>% rename("OverdoseDeaths" = "Data.Value")
df <- subset(df, select = c(State.Name, Year, OverdoseDeaths))
df <- left_join(df, subset(ds, select = c(State.Name, Year, TotalDeaths)))

# Share of overdose deaths in total mortality
df <- df %>% mutate(PropDeaths = OverdoseDeaths / TotalDeaths, .after = Year)

# Assign ID to each state
df <- df %>% 
  group_by(State.Name) %>% 
  mutate(State_ID = cur_group_id()) %>% 
  arrange(State_ID)


# Auxiliary Data and Processing -------------------------------------------

# Load
dx <- read_excel("Data/auxiliary_data.xlsx")

# Merge to df
df <- left_join(df, dx)

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

# Log OverdoseDeaths
df <- df %>% mutate(LogOverdoseDeaths = log(OverdoseDeaths), .after = OverdoseDeaths)

# Log AvgTemp
df <- df %>% mutate(LogAvgTemp = log(AvgTemp), .after = AvgTemp)


# Opioid Prescribing Rate Data --------------------------------------------

# Read data
dz <- read.csv("Data/medicaid_opioid_prescribing_rates.csv")

# Plan_Type = "All"
dz <- subset(dz, Plan_Type == "All")

# Select columns
dz <- subset(dz, select = c(Geo_Desc, Year, Opioid_Prscrbng_Rate))

# Rename stuff
dz <- dz %>% rename("State.Name" = "Geo_Desc", "OpioidPrescribingRate" = "Opioid_Prscrbng_Rate")

# Merge to df
df <- left_join(df, dz)

# Impute missing data using random forest
set.seed(123)
df$PoliticalLeaning <- as.factor(df$PoliticalLeaning)
df <- cbind(df[, 1], missForest(data.frame(df)[, 2:30])[["ximp"]])

# Average across the years
df <- df %>% group_by(State_ID) %>% mutate(OpioidPrescribingRate = mean(OpioidPrescribingRate))
df$OpioidPrescribingRate <- round(df$OpioidPrescribingRate, 3)


# Distribution of outcome variables ---------------------------------------

pdf(file = "Figures/histogram.pdf", height = 4, width = 6)
par(mfrow = c(1, 2))
hist(df$LogOverdoseDeaths, main = "", xlab = "Log Overdose Deaths",
     cex.lab = 0.8, cex.axis = 0.8)
hist(df$PropDeaths, main = "", xlab = "Prop. Deaths",
     cex.lab = 0.8, cex.axis = 0.8)
dev.off()


# Plots by year -----------------------------------------------------------

# Average overdose deaths and prop deaths per year
dn <- df %>% 
  group_by(Year) %>% 
  summarise(AverageOverdoseDeaths = log(mean(OverdoseDeaths)),
            AveragePropDeaths = mean(PropDeaths))

scaleFactor <- max(dn$AverageOverdoseDeaths) / max(dn$AveragePropDeaths)

pdf(file = "Figures/deaths_prop_by_year.pdf", height = 4, width = 6)
ggplot(dn, aes(x = Year)) +
  geom_point(aes(y = AverageOverdoseDeaths)) +
  geom_line(aes(y = AverageOverdoseDeaths), group = 1) +
  geom_point(aes(y = AveragePropDeaths * scaleFactor), color = "blue") +
  geom_line(aes(y = AveragePropDeaths * scaleFactor), group = 1, color = "blue") +
  theme_classic(base_size = 12) +
  scale_fill_brewer('', palette = 'Dark2') +
  labs(x = "") +
  scale_y_continuous(name = "Avg. Overdose Deaths",
                     sec.axis = sec_axis(~. / scaleFactor, name = "Avg. Prop. Deaths")) +
  theme(strip.background = element_blank(),
        strip.placement = 'outside',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = element_blank(),
        axis.line.y.right = element_line(color = "blue"),
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "blue")) +
  scale_x_continuous(breaks = seq(2015, 2023, 1))
dev.off()

# Average overdose deaths in control and treatment states
dn <- df %>% 
  group_by(Year, Law) %>% 
  summarise(AverageOverdoseDeaths = log(mean(OverdoseDeaths)),
            AveragePropDeaths = mean(PropDeaths))

# Average overdose deaths
p <- dn %>% 
  ggplot(aes(x = Year, y = AverageOverdoseDeaths, color = as.factor(Law))) +
  geom_point() +
  geom_line() +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = seq(2015, 2023, 1)) +
  labs(x = "", y = "Avg. Overdose Deaths") +
  scale_color_discrete(name = "",
                      breaks = c(0, 1),
                      labels = c("Control (n = 13)", "Treatment (n = 38)"))

# Average prop deaths
q <- dn %>% 
  ggplot(aes(x = Year, y = AveragePropDeaths, color = as.factor(Law))) +
  geom_point() +
  geom_line() +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = seq(2015, 2023, 1)) +
  labs(x = "", y = "Avg. Prop. Deaths") +
  scale_color_discrete(name = "",
                       breaks = c(0, 1),
                       labels = c("Control (n = 13)", "Treatment (n = 38)"))

figure <- ggarrange(p, q, common.legend = T, legend = "bottom")
pdf(file = "Figures/deaths_prop_tc.pdf", height = 4.5, width = 9)
figure
dev.off()


# Map ---------------------------------------------------------------------

# Load
states_sf <- get_urbn_map("states", sf = T)
states_sf <- states_sf %>% rename("State.Name" = "state_name")

# Merge to auxiliary data dz to plot
dx <- left_join(dx, subset(states_sf, select = c(State.Name, geometry)))

# LawDate
dx$LawDateforMap <- as.factor(dx$LawDateforMap)
dx <- dx %>% mutate(LawDateforMap = case_when(LawDateforMap == 0 ~ "No Law", T ~ LawDateforMap))
dx <- st_as_sf(dx)
ggplot(dx) +
  geom_sf(aes(fill = LawDateforMap)) +
  theme_void() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#C4E2A2", "#8EC68B", "#54AA79", "#008D6C", "white"))

# AnyPolicyDate
dx$AnyPolicyDateforMap <- as.factor(dx$AnyPolicyDateforMap)
dx <- dx %>% 
  mutate(AnyPolicyDateforMap = case_when(AnyPolicyDateforMap == 0 ~ "No Law / Policy", T ~ AnyPolicyDateforMap))
ggplot(dx) +
  geom_sf(aes(fill = AnyPolicyDateforMap)) +
  theme_void() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#F8FFBF", "#C4E2A2", "#8EC68B", "#54AA79", "#008D6C", "white"))

# ExistingPDMP
ggplot(dx) +
  geom_sf(aes(fill = factor(ExistingPDMP))) +
  theme_void() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("white", "#C4E2A2"))


# Summary Statistics ------------------------------------------------------

# stargazer(data.frame(df))


# Event study -------------------------------------------------------------

# ~ExistingPolicy + ExistingPDMP + MedicaidPolicy + PhysicianDensity + 
# OpioidPrescribingRate + AgeAdjDeathRate + PoliticalLeaning + LogAvgTemp

out <- att_gt(yname = "LogOverdoseDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              xformla = ~ExistingPolicy + ExistingPDMP + MedicaidPolicy + PhysicianDensity + 
                OpioidPrescribingRate + AgeAdjDeathRate + PoliticalLeaning + LogAvgTemp,
              data = df,
              alp = 0.05)

# Overall average treatment effect
summary(aggte(out, type = "group", na.rm = T))

# Event study plot
p <- ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  theme_classic(base_size = 20) +
  ylim(-0.03, 0.03) +
  ggtitle("(5)") +
  theme(plot.title = element_text(hjust = 0.5))
saveRDS(p, file = "Figures/Event Study/prop_deaths_env_covariates.RDS")


# Panel Plot --------------------------------------------------------------

figure <- ggarrange(readRDS("Figures/Event Study/prop_deaths_no_covariates.RDS"),
                    readRDS("Figures/Event Study/prop_deaths_policy_covariates.RDS"),
                    readRDS("Figures/Event Study/prop_deaths_health_covariates.RDS"),
                    readRDS("Figures/Event Study/prop_deaths_dem_covariates.RDS"),
                    readRDS("Figures/Event Study/prop_deaths_env_covariates.RDS"),
                    common.legend = T, legend = "bottom")
pdf(file = "Figures/Event Study/prop_deaths_panel_plot.pdf", height = 10, width = 15)
figure
dev.off()

