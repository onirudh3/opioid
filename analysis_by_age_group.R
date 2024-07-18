
# Libraries ---------------------------------------------------------------




# 15-24 -------------------------------------------------------------------

# Read
df <- read.csv("Data/Deaths by Age Group/deaths_15_to_24.csv")

# Model
out <- att_gt(yname = "LogDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              xformla = ~ExistingPolicy + ExistingPDMP + MedicaidPolicy + PhysicianDensity + 
                OpioidPrescribingRate + AgeAdjDeathRate + PoliticalLeaning + AvgTemp,
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