
# Libraries ---------------------------------------------------------------

library(did)
library(ggpubr)
library(ggplot2)


# 15-24 -------------------------------------------------------------------

# xformla = ~ExistingPolicy + ExistingPDMP + MedicaidPolicy + PhysicianDensity +
#   OpioidPrescribingRate + PoliticalLeaning + LogAvgTemp,

df <- read.csv("Data/Deaths by Age Group/deaths_15_to_24.csv")
out <- att_gt(yname = "LogDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              data = df,
              xformla = ~1,
              control_group = "notyettreated",
              alp = 0.05)
summary(aggte(out, type = "group", na.rm = T))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  theme_classic(base_size = 20) +
  ylim(-4.3, 4.3) +
  ggtitle("15-24") +
  theme(plot.title = element_text(hjust = 0.5))


# 25-34 -------------------------------------------------------------------

df <- read.csv("Data/Deaths by Age Group/deaths_25_to_34.csv")
out <- att_gt(yname = "LogDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              data = df,
              xformla = ~ExistingPolicy + ExistingPDMP + MedicaidPolicy + PhysicianDensity +
                OpioidPrescribingRate + PoliticalLeaning + LogAvgTemp,
              control_group = "notyettreated",
              alp = 0.05)
summary(aggte(out, type = "group", na.rm = T))
q <- ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  theme_classic(base_size = 20) +
  ylim(-4.3, 4.3) +
  ggtitle("25-34") +
  theme(plot.title = element_text(hjust = 0.5))


# 35-44 -------------------------------------------------------------------

df <- read.csv("Data/Deaths by Age Group/deaths_35_to_44.csv")
out <- att_gt(yname = "LogDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              data = df,
              xformla = ~ExistingPolicy + ExistingPDMP + MedicaidPolicy + PhysicianDensity +
                OpioidPrescribingRate + PoliticalLeaning + LogAvgTemp,
              control_group = "notyettreated",
              alp = 0.05)
summary(aggte(out, type = "group", na.rm = T))
r <- ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  theme_classic(base_size = 20) +
  ylim(-4.3, 4.3) +
  ggtitle("35-44") +
  theme(plot.title = element_text(hjust = 0.5))


plot(df$PhysicianDensity, df$OpioidPrescribingRate, pch = 20)
abline(lm(df$OpioidPrescribingRate ~ df$PhysicianDensity, data = df), col = "blue")


# 45-54 -------------------------------------------------------------------

df <- read.csv("Data/Deaths by Age Group/deaths_45_to_54.csv")
out <- att_gt(yname = "LogDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              data = df,
              xformla = ~ExistingPolicy + ExistingPDMP + MedicaidPolicy + PhysicianDensity +
                OpioidPrescribingRate + PoliticalLeaning + LogAvgTemp,
              control_group = "notyettreated",
              alp = 0.05)
summary(aggte(out, type = "group", na.rm = T))
s <- ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  theme_classic(base_size = 20) +
  ylim(-4.3, 4.3) +
  ggtitle("45-54") +
  theme(plot.title = element_text(hjust = 0.5))


# 55-64 -------------------------------------------------------------------

df <- read.csv("Data/Deaths by Age Group/deaths_55_to_64.csv")
out <- att_gt(yname = "LogDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              data = df,
              xformla = ~ExistingPolicy + ExistingPDMP + MedicaidPolicy + PhysicianDensity +
                OpioidPrescribingRate + PoliticalLeaning + LogAvgTemp,
              control_group = "notyettreated",
              alp = 0.05)
summary(aggte(out, type = "group", na.rm = T))
t <- ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  theme_classic(base_size = 20) +
  ylim(-4.3, 4.3) +
  ggtitle("55-64") +
  theme(plot.title = element_text(hjust = 0.5))


# 65-74 -------------------------------------------------------------------

df <- read.csv("Data/Deaths by Age Group/deaths_65_to_74.csv")
out <- att_gt(yname = "LogDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              data = df,
              xformla = ~ExistingPolicy + ExistingPDMP + MedicaidPolicy + PhysicianDensity +
                OpioidPrescribingRate + PoliticalLeaning + LogAvgTemp,
              control_group = "notyettreated",
              alp = 0.05)
summary(aggte(out, type = "group", na.rm = T))
u <- ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  theme_classic(base_size = 20) +
  ylim(-4.3, 4.3) +
  ggtitle("65-74") +
  theme(plot.title = element_text(hjust = 0.5))


# Panel plot all covariates -----------------------------------------------

figure <- ggarrange(p, q, r, s, t, u, common.legend = T, legend = "bottom")
pdf(file = "Figures/Event Study/att_by_age_all_covariates.pdf", height = 10, width = 15)
figure
dev.off()


# Panel plot no covariates ------------------------------------------------

figure <- ggarrange(p, q, r, s, t, u, common.legend = T, legend = "bottom")
pdf(file = "Figures/Event Study/att_by_age_no_covariates.pdf", height = 10, width = 15)
figure
dev.off()


