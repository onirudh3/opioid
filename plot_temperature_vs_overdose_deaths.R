dn <- df %>% 
  group_by(State.Name) %>% 
  summarise(AvgTemp = AvgTemp, OverdoseDeaths = mean(OverdoseDeaths)) %>% 
  distinct()

dn$logOverdoseDeaths <- log(dn$OverdoseDeaths)

dn$logAvgTemp <- log(dn$AvgTemp)

ggplot(dn, aes(logAvgTemp, logOverdoseDeaths)) +
  geom_point()
