# Average overdose deaths by political leaning
dn <- df %>% 
  group_by(Year, PoliticalLeaning) %>% 
  summarise(AverageOverdoseDeaths = mean(OverdoseDeaths),
            AveragePropDeaths = mean(PropDeaths))

# Plot
p <- dn %>% 
  ggplot(aes(x = Year, y = AverageOverdoseDeaths, color = as.factor(PoliticalLeaning))) +
  geom_point() +
  geom_line() +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = seq(2015, 2023, 1)) +
  labs(x = "", y = "Avg. Overdose Deaths") +
  scale_color_discrete(name = "",
                       breaks = c("Democrat", "Republican", "Swing State"),
                       labels = c("Democrat (n = 19)", "Republican (n = 22)", "Swing State (n = 10)"))


q <- dn %>% 
  ggplot(aes(x = Year, y = AveragePropDeaths, color = as.factor(PoliticalLeaning))) +
  geom_point() +
  geom_line() +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = seq(2015, 2023, 1)) +
  labs(x = "", y = "Avg. Prop. Deaths") +
  scale_color_discrete(name = "",
                       breaks = c("Democrat", "Republican", "Swing State"),
                       labels = c("Democrat (n = 19)", "Republican (n = 22)", "Swing State (n = 10)"))

figure <- ggarrange(p, q, common.legend = T, legend = "bottom")
pdf(file = "Figures/deaths_prop_by_political.pdf", height = 4.5, width = 9)
figure
dev.off()
