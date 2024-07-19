
# Libraries ---------------------------------------------------------------

library(did)
library(ggplot2)


# 15-24 -------------------------------------------------------------------

# Read
df <- read.csv("Data/Deaths by Age Group/deaths_15_to_24.csv")

# Model
out <- att_gt(yname = "LogDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              xformla = ~ExistingPolicy + ExistingPDMP + MedicaidPolicy + PhysicianDensity + 
                OpioidPrescribingRate + PoliticalLeaning + LogAvgTemp,
              data = df,
              # control_group = "notyettreated",
              alp = 0.05)

# Overall average treatment effect
summary(aggte(out, type = "group", na.rm = T))

# Event study plot
ggdid(aggte(out, type = "dynamic", na.rm = T))







