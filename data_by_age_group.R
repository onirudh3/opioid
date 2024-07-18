
# Libraries ---------------------------------------------------------------

library(dplyr)


# Data --------------------------------------------------------------------

# ICD-10 codes for opioid deaths -- 
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

# 15-24
write.csv(subset(df, Ten.Year.Age.Groups.Code == "15-24"), 
          file = "Data/Deaths by Age Group/deaths_15_to_24", row.names = F)

