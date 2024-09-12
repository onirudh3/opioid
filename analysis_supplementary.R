
# Effect of medicaid policy implementation --------------------------------

out <- att_gt(yname = "LogOverdoseDeaths",
              gname = "MedicaidPolicyDate",
              idname = "State_ID",
              tname = "Year",
              xformla = ~1,
              data = df,
              alp = 0.05)

summary(aggte(out, type = "group", na.rm = T))

pdf(file = "Figures/Event Study/medicaid_effect.pdf", height = 5, width = 7)
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("ATT (95% Conf. Int.) = 0.062 (-0.039, 0.164)") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


# Effect of PDMP ----------------------------------------------------------

# PDMP = 0
out <- att_gt(yname = "LogOverdoseDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              xformla = ~1,
              data = subset(df, ExistingPDMP == 0),
              alp = 0.05)

summary(aggte(out, type = "group", na.rm = T))

p <- ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("PDMP = 0") +
  geom_text(x = 1, y = 0.6, label = "ATT (95% Conf. Int.) = -0.102 (-0.210, 0.007)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.7, 0.7)

# PDMP = 1
out <- att_gt(yname = "LogOverdoseDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              xformla = ~1,
              control_group="notyettreated",
              data = subset(df, ExistingPDMP == 1),
              alp = 0.05)

summary(aggte(out, type = "group", na.rm = T))

q <- ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("PDMP = 1") +
  geom_text(x = 2, y = 0.6, label = "ATT (95% Conf. Int.) = -0.087 (-0.181, 0.007)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.7, 0.7)

figure <- ggarrange(p, q, common.legend = T, legend = "bottom")
pdf(file = "Figures/Event Study/pdmp_effect.pdf", height = 5, width = 10)
figure
dev.off()


# Above and below mean day supply -----------------------------------------

# Below mean
out <- att_gt(yname = "LogOverdoseDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              xformla = ~1,
              data = subset(df, DaySupplyMean == 1),
              alp = 0.05)

summary(aggte(out, type = "group", na.rm = T))

p <- ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Below Day Supply Mean") +
  geom_text(x = 1, y = 0.6, label = "ATT (95% Conf. Int.) = -0.093 (-0.200, 0.012)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.8, 0.8)

# Above mean
out <- att_gt(yname = "LogOverdoseDeaths",
              gname = "LawDate",
              idname = "State_ID",
              tname = "Year",
              xformla = ~1,
              control_group = "notyettreated",
              data = subset(df, DaySupplyMean == 2),
              alp = 0.05)

summary(aggte(out, type = "group", na.rm = T))

q <- ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Above Day Supply Mean") +
  geom_text(x = 1, y = 0.6, label = "ATT (95% Conf. Int.) = -0.060 (-0.257, 0.137)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.8, 0.8)

figure <- ggarrange(p, q, common.legend = T, legend = "bottom")
pdf(file = "Figures/Event Study/day_supply_effect.pdf", height = 5, width = 10)
figure
dev.off()
